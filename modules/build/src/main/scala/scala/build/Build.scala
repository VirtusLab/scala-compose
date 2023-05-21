package scala.build

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j.TaskFinishParams
import com.swoval.files.FileTreeViews.Observer
import com.swoval.files.{PathWatcher, PathWatchers}
import dependency.ScalaParameters
import jdk.javadoc.internal.doclets.toolkit.BaseOptions

import java.io.File
import java.nio.file.FileSystemException
import java.util.UUID
import java.util.concurrent.{ScheduledExecutorService, ScheduledFuture}
import scala.annotation.tailrec
import scala.build.EitherCps.{either, value}
import scala.build.Ops.*
import scala.build.compiler.{ScalaCompiler, ScalaCompilerMaker}
import scala.build.errors.*
import scala.build.input.VirtualScript.VirtualScriptNameRegex
import scala.build.input.*
import scala.build.internal.resource.ResourceMapper
import scala.build.internal.{Constants, MainClass, Name, Util}
import scala.build.options.ScalaVersionUtil.asVersion
import scala.build.options.*
import scala.build.options.validation.ValidationException
import scala.build.postprocessing.LineConversion.scalaLineToScLineShift
import scala.build.postprocessing.*
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal
import scala.util.{Properties, Try}

trait Build {
  def inputs: Inputs
  def options: BuildOptions
  def scope: Scope
  def outputOpt: Option[os.Path]
  def success: Boolean
  def diagnostics: Option[Seq[(Either[String, os.Path], bsp4j.Diagnostic)]]

  def successfulOpt: Option[Build.Successful]
}

object Build {

  final case class Successful(
    inputs: Inputs,
    options: BuildOptions,
    scalaParams: Option[ScalaParameters],
    scope: Scope,
    sources: Sources,
    artifacts: Artifacts,
    project: Project,
    output: os.Path,
    diagnostics: Option[Seq[(Either[String, os.Path], bsp4j.Diagnostic)]],
    generatedSources: Seq[GeneratedSource],
    isPartial: Boolean,
    refreshed: Boolean
  ) extends Build {
    def success: Boolean                  = true
    def successfulOpt: Some[this.type]    = Some(this)
    def outputOpt: Some[os.Path]          = Some(output)
    def dependencyClassPath: Seq[os.Path] = sources.resourceDirs ++ artifacts.classPath
    def fullClassPath: Seq[os.Path]       = Seq(output) ++ dependencyClassPath
    def foundMainClasses(): Seq[String] =
      MainClass.find(output).sorted ++
        options.classPathOptions.extraClassPath.flatMap(MainClass.find).sorted
    def retainedMainClass(
      mainClasses: Seq[String],
      commandString: String,
      logger: Logger
    ): Either[BuildException, String] = {
      val defaultMainClassOpt = sources.defaultMainClass
        .filter(name => mainClasses.contains(name))
      def foundMainClass: Either[BuildException, String] =
        mainClasses match {
          case Seq()          => Left(new NoMainClassFoundError)
          case Seq(mainClass) => Right(mainClass)
          case _ =>
            inferredMainClass(mainClasses, logger)
              .left.flatMap { mainClasses =>
                // decode the names to present them to the user,
                // but keep the link to each original name to account for package prefixes:
                // "pack.Main$minus1" decodes to "pack.Main-1", which encodes back to "pack$u002EMain$minus1"
                //  ^^^^^^^^^^^^^^^^----------------NOT THE SAME-----------------------^^^^^^^^^^^^^^^^^^^^^
                val decodedToEncoded = mainClasses.map(mc => Name.decoded(mc) -> mc).toMap

                options.interactive.flatMap { interactive =>
                  interactive
                    .chooseOne(
                      "Found several main classes. Which would you like to run?",
                      decodedToEncoded.keys.toList
                    )
                    .map(decodedToEncoded(_)) // encode back the name of the chosen class
                    .toRight {
                      SeveralMainClassesFoundError(
                        ::(mainClasses.head, mainClasses.tail.toList),
                        commandString,
                        Nil
                      )
                    }
                }
              }
        }

      defaultMainClassOpt match {
        case Some(cls) => Right(cls)
        case None      => foundMainClass
      }
    }
    private def inferredMainClass(
      mainClasses: Seq[String],
      logger: Logger
    ): Either[Seq[String], String] = {
      val scriptInferredMainClasses =
        sources.inMemory.map(im => im.originalPath.map(_._1))
          .flatMap {
            case Right(originalRelPath) if originalRelPath.toString.endsWith(".sc") =>
              Some {
                originalRelPath
                  .toString
                  .replace(".", "_")
                  .replace("/", ".")
              }
            case Left(VirtualScriptNameRegex(name)) => Some(s"${name}_sc")
            case _                                  => None
          }
      val filteredMainClasses =
        mainClasses.filter(!scriptInferredMainClasses.contains(_))
      if (filteredMainClasses.length == 1) {
        val pickedMainClass = filteredMainClasses.head
        if (scriptInferredMainClasses.nonEmpty) {
          val firstScript   = scriptInferredMainClasses.head
          val scriptsString = scriptInferredMainClasses.mkString(", ")
          logger.message(
            s"Running $pickedMainClass. Also detected script main classes: $scriptsString"
          )
          logger.message(
            s"You can run any one of them by passing option --main-class, i.e. --main-class $firstScript"
          )
          logger.message(
            "All available main classes can always be listed by passing option --list-main-classes"
          )
        }
        Right(pickedMainClass)
      }
      else
        Left(mainClasses)
    }
    def retainedMainClassOpt(
      mainClasses: Seq[String],
      logger: Logger
    ): Option[String] = {
      val defaultMainClassOpt = sources.defaultMainClass
        .filter(name => mainClasses.contains(name))
      def foundMainClass =
        mainClasses match {
          case Seq()          => None
          case Seq(mainClass) => Some(mainClass)
          case _              => inferredMainClass(mainClasses, logger).toOption
        }

      defaultMainClassOpt.orElse(foundMainClass)
    }

    def crossKey: CrossKey = {
      val optKey = scalaParams.map { params =>
        BuildOptions.CrossKey(
          params.scalaVersion,
          options.platform.value
        )
      }
      CrossKey(optKey, scope)
    }
  }

  final case class Failed(
    inputs: Inputs,
    options: BuildOptions,
    scope: Scope,
    sources: Sources,
    artifacts: Artifacts,
    project: Project,
    diagnostics: Option[Seq[(Either[String, os.Path], bsp4j.Diagnostic)]]
  ) extends Build {
    def success: Boolean         = false
    def successfulOpt: None.type = None
    def outputOpt: None.type     = None
  }

  final case class Cancelled(
    inputs: Inputs,
    options: BuildOptions,
    scope: Scope,
    reason: String
  ) extends Build {
    def success: Boolean         = false
    def successfulOpt: None.type = None
    def outputOpt: None.type     = None
    def diagnostics: None.type   = None
  }

  /** If some options are manually overridden, append a hash of the options to the project name
    * Using only the command-line options not the ones from the sources.
    */
  def updateInputs(
    inputs: Inputs,
    options: BuildOptions,
    testOptions: Option[BuildOptions] = None
  ): Inputs = {

    // If some options are manually overridden, append a hash of the options to the project name
    // Using options, not options0 - only the command-line options are taken into account. No hash is
    // appended for options from the sources.
    val optionsHash     = options.hash
    val testOptionsHash = testOptions.flatMap(_.hash)

    inputs.copy(
      baseProjectName =
        inputs.baseProjectName
          + optionsHash.map("_" + _).getOrElse("")
          + testOptionsHash.map("_" + _).getOrElse("")
    )
  }

  final case class ResourceGenerator(
    module: String,
    shouldPackage: Logger => Either[BuildException, Boolean],
    doPackage: (Logger, Build.Successful) => Either[BuildException, Unit]
  )

  final case class BuildModule(
    inputs: Inputs,
    projectName: String,
    platforms: List[Platform],
    dependsOn: List[String],
    resourceGenerators: List[ResourceGenerator]
  )
  private final case class CrossBuildModule(
    module: BuildModule,
    crossSources: CrossSources,
    allInputs: Inputs,
    sharedOptions: BuildOptions,
    crossOptions: Seq[BuildOptions],
    platform: Option[Platform]
  ) {
    lazy val bloopName: String = platform match
      case Some(platform) =>
        if platform == Platform.JVM then module.projectName
        else s"${module.projectName}-${Platform.normalize(platform.repr)}"
      case None => module.inputs.projectName

    def scopedSources(baseOptions: BuildOptions): Either[BuildException, ScopedSources] =
      crossSources.scopedSources(baseOptions)
  }

  private final case class PreBuild(cm: CrossBuildModule, buildDependencies: List[String])

  private object PreBuild {
    given asModule: ModuleLike[PreBuild] with {
      def id(t: PreBuild): String = t.cm.bloopName

      // def weight(t: PreBuild): Int =
      //   1 // if some targets take longer to build, we could factor that in here

      def dependsOn(t: PreBuild): Seq[String] =
        t.buildDependencies
    }
  }

  private def build(
    modules: Seq[BuildModule],
    options: BuildOptions,
    logger: Logger,
    buildClient: BloopBuildClient,
    compiler: ScalaCompiler,
    docCompilerOpt: Option[ScalaCompiler],
    crossBuilds: Boolean,
    buildTests: Boolean,
    partial: Option[Boolean],
    actionableDiagnostics: Option[Boolean],
    configDir: Option[os.Path]
  )(using ScalaCliInvokeData): Either[BuildException, Map[String, Seq[Builds]]] = either {

    val crossModules = modules.flatMap { module =>
      // allInputs contains elements from using directives
      val (crossSources, allInputs) = value {
        CrossSources.forInputs(
          module.inputs,
          Sources.defaultPreprocessors(
            options.scriptOptions.codeWrapper.getOrElse(CustomCodeWrapper),
            options.archiveCache,
            options.internal.javaClassNameVersionOpt,
            () => options.javaHome().value.javaCommand
          ),
          logger,
          options.suppressWarningOptions,
          options.internal.exclude
        )
      }
      val sharedOptions: BuildOptions     = crossSources.sharedOptions(options)
      val crossOptions: Seq[BuildOptions] = sharedOptions.crossOptions

      def makeModule(platform: Option[Platform]) =
        CrossBuildModule(module, crossSources, allInputs, sharedOptions, crossOptions, platform)

      if module.platforms.isEmpty then
        makeModule(None) :: Nil
      else
        module.platforms.map(p => makeModule(Some(p)))
    }

    val crossModuleLookup = crossModules.map(cm => (cm.module.projectName, cm.platform) -> cm).toMap

    val preBuilds = crossModules.map { cm =>
      val mainDependsOn =
        val resourceDependencies = cm.module.resourceGenerators.map(rg =>
          // TODO: optimise
          val candidates = crossModules.filter(_.module.projectName == rg.module)
          assert(
            candidates.sizeIs == 1
          ) // we should enforce that application modules can only have a single platform
          candidates.head.bloopName
        )
        val libraryDeps = cm.module.dependsOn.flatMap(d =>
          crossModuleLookup.get((d, cm.platform)).map(_.bloopName)
        )
        libraryDeps ++ resourceDependencies
      PreBuild(cm, mainDependsOn)
    }

    def doPostProcess(build: Build, inputs: Inputs, scope: Scope): Unit = build match {
      case build: Build.Successful =>
        for (sv <- build.project.scalaCompiler.map(_.scalaVersion))
          postProcess(
            build.generatedSources,
            inputs.generatedSrcRoot(scope),
            build.output,
            logger,
            inputs.workspace,
            updateSemanticDbs = true,
            scalaVersion = sv
          ).left.foreach(_.foreach(logger.message(_)))
      case _ =>
    }

    final case class NonCrossBuilds(
      main: Build,
      testOpt: Option[Build],
      docOpt: Option[Build],
      testDocOpt: Option[Build]
    )

    extension (cm: CrossBuildModule)
      def resourcesDir: os.Path =
        configDir match
          case Some(workspace) =>
            Build.resourcesRootDir(workspace, cm.module.projectName)
          case None =>
            Build.resourcesRootDir(
              cm.allInputs.workspace,
              cm.allInputs.projectName
            )

      def scopedClassesDir(scope: Scope, platform: Option[Platform]): os.Path =
        configDir match
          case Some(workspace) =>
            Build.classesDir(workspace, cm.module.projectName, scope, optPlatform = platform)
          case None =>
            Build.classesDir(
              cm.allInputs.workspace,
              cm.allInputs.projectName,
              scope
            )

      def depOptions(
        baseOptions: BuildOptions,
        scope: Scope,
        platform: Positioned[Platform]
      ): Either[BuildException, BuildOptions] =
        def getDepOptions(options: BuildOptions, optClassDir: Option[os.Path]) =
          BuildOptions(
            scalaOptions = ScalaOptions(platform = Some(platform)),
            classPathOptions =
              optClassDir.fold(options.classPathOptions) { dir =>
                options.classPathOptions.copy(extraClassPath =
                  dir +: options.classPathOptions.extraClassPath
                )
              }
          )

        def step(acc: BuildOptions, dep: String): Either[BuildException, BuildOptions] = either {
          val depM = crossModuleLookup((dep, cm.platform))
          val classesDir0 =
            if scope == Scope.Test then None
            else Some(depM.scopedClassesDir(scope, depM.platform))
          val sharedOpts     = getDepOptions(depM.crossSources.sharedOptions(acc), classesDir0)
          val scopedSources0 = value(depM.scopedSources(acc))
          val acc1 =
            scopedSources0
              .buildOptions
              .flatMap(_.valueFor(scope))
              .foldRight(sharedOpts)((d, acc) => getDepOptions(d, None).orElse(acc))
          value(depM.depOptions(acc1, scope, platform))
        }

        either {
          cm.module.dependsOn.foldLeft(baseOptions)((acc, d) => value(step(acc, d)))
        }
      end depOptions

    def doBuild(
      b: PreBuild,
      overrideOptions: BuildOptions
    ): Either[BuildException, NonCrossBuilds] = either {
      import b.cm.{sharedOptions, crossSources, allInputs}

      val baseOptions = overrideOptions.orElse(sharedOptions)

      val scopedSources = value(crossSources.scopedSources(baseOptions))

      val mainSources0 = value(scopedSources.sources(Scope.Main, baseOptions orElse mainDeps, allInputs.workspace))
      val testSources0 = value(scopedSources.sources(Scope.Test, baseOptions orElse testDeps, allInputs.workspace))

      val platformArg0 =
        b.cm.platform.map(p => Positioned(List(Position.Custom("DEFAULT-COMPOSE")), p))

      val modulePlatform = platformArg0.getOrElse(baseOptions.platform)

      val mainDeps = value(b.cm.depOptions(baseOptions, Scope.Main, modulePlatform))
      val testDeps = value(b.cm.depOptions(baseOptions, Scope.Test, modulePlatform))

      def mixInPlatform(baseOptions: BuildOptions) = baseOptions.copy(
        scalaOptions = baseOptions.scalaOptions.copy(platform = Some(modulePlatform))
      )

      def addResource(sources: Sources, resourceDir: os.Path): Sources =
        sources.copy(resourceDirs = resourceDir +: sources.resourceDirs)

      val (mainSources, testSources) =
        if b.cm.module.resourceGenerators.isEmpty then
          (mainSources0, testSources0)
        else
          val resourceDir = b.cm.resourcesDir
          (addResource(mainSources0, resourceDir), addResource(testSources0, resourceDir))

      val mainOptions = mixInPlatform(mainSources.buildOptions)
      val testOptions = mixInPlatform(testSources.buildOptions)

      val inputs0 = updateInputs(
        allInputs,
        mainOptions, // update hash in inputs with options coming from the CLI or cross-building, not from the sources
        Some(testOptions).filter(_ != mainOptions)
      )

      def doBuildScope(
        options: BuildOptions,
        sources: Sources,
        scope: Scope,
        dependsOn: List[String],
        actualCompiler: ScalaCompiler = compiler
      ): Either[BuildException, Build] =
        either {
          val sources0 = sources.withVirtualDir(inputs0, scope, options)

          val generatedSources = sources0.generateSources(inputs0.generatedSrcRoot(scope))

          val res = build(
            inputs0,
            sources0,
            generatedSources,
            options,
            scope,
            logger,
            buildClient,
            actualCompiler,
            buildTests,
            partial,
            actionableDiagnostics,
            moduleProjectName = b.cm.platform.map(_ => b.cm.module.projectName),
            optPlatform = b.cm.platform,
            dependsOn = dependsOn,
            workspace = configDir
          )

          value(res)
        }

      val mainBuild = value(doBuildScope(
        mainOptions,
        mainSources,
        Scope.Main,
        dependsOn = b.buildDependencies
      ))
      val mainDocBuildOpt = docCompilerOpt match {
        case None => None
        case Some(docCompiler) =>
          Some(value(doBuildScope(
            mainOptions,
            mainSources,
            Scope.Main,
            dependsOn = b.buildDependencies,
            actualCompiler = docCompiler
          )))
      }

      def testBuildOpt(doc: Boolean = false): Either[BuildException, Option[Build]] = either {
        if (buildTests) {
          val actualCompilerOpt =
            if (doc) docCompilerOpt
            else Some(compiler)
          actualCompilerOpt match {
            case None => None
            case Some(actualCompiler) =>
              val testBuild = value {
                mainBuild match {
                  case s: Build.Successful =>
                    val extraTestOptions = BuildOptions(
                      classPathOptions = ClassPathOptions(
                        extraClassPath = Seq(s.output)
                      )
                    )
                    val testOptions0 = extraTestOptions.orElse(testOptions)
                    doBuildScope(
                      testOptions0,
                      testSources,
                      Scope.Test,
                      dependsOn = Nil,
                      actualCompiler = actualCompiler
                    )
                  case _ =>
                    Right(Build.Cancelled(
                      b.cm.module.inputs,
                      sharedOptions,
                      Scope.Test,
                      "Parent build failed or cancelled"
                    ))
                }
              }
              Some(testBuild)
          }
        }
        else None
      }

      val testBuildOpt0 = value(testBuildOpt())
      doPostProcess(mainBuild, inputs0, Scope.Main)
      for (testBuild <- testBuildOpt0)
        doPostProcess(testBuild, inputs0, Scope.Test)

      val docTestBuildOpt0 = value(testBuildOpt(doc = true))

      NonCrossBuilds(mainBuild, testBuildOpt0, mainDocBuildOpt, docTestBuildOpt0)
    }

    def buildScopes(b: PreBuild): Either[BuildException, Builds] =
      either {
        import b.cm.crossOptions

        val nonCrossBuilds = value(doBuild(b, BuildOptions()))

        val (extraMainBuilds, extraTestBuilds, extraDocBuilds, extraDocTestBuilds) =
          if (crossBuilds) {
            val extraBuilds = value {
              val maybeBuilds = crossOptions.map(doBuild(b, _))

              maybeBuilds
                .sequence
                .left.map(CompositeBuildException(_))
            }
            (
              extraBuilds.map(_.main),
              extraBuilds.flatMap(_.testOpt),
              extraBuilds.flatMap(_.docOpt),
              extraBuilds.flatMap(_.testDocOpt)
            )
          }
          else
            (Nil, Nil, Nil, Nil)

        Builds(
          Seq(nonCrossBuilds.main) ++ nonCrossBuilds.testOpt.toSeq,
          Seq(extraMainBuilds, extraTestBuilds),
          nonCrossBuilds.docOpt.toSeq ++ nonCrossBuilds.testDocOpt.toSeq,
          Seq(extraDocBuilds, extraDocTestBuilds)
        )
      }

    logger.debug {
      val preBuildsD = preBuilds.map { p =>
        s"${p.cm.bloopName}: ${p.buildDependencies.mkString(", ")}"
      }.mkString("\n")
      s"Pre-builds:\n$preBuildsD"
    }

    val preBuilds0 = Dag.topologicalSort(preBuilds) // TODO: parallelize steps

    logger.debug {
      val debugged = preBuilds0.zipWithIndex.map { (stage, i) =>
        s"$i: ${stage.map(_.cm.bloopName).mkString(", ")}"
      }.mkString("\n")
      s"Build stages:\n$debugged"
    }

    val res = preBuilds0.flatten.foldLeft(Map.empty[String, Builds]) { (acc, b) =>
      for generator <- b.cm.module.resourceGenerators do {
        // TODO: optimise
        val targetName = crossModules.find(_.module.projectName == generator.module).get.bloopName
        acc(targetName).main match
          case build: Successful =>
            if build.refreshed || value(generator.shouldPackage(logger)) then
              val taskId    = new bsp4j.TaskId(UUID.randomUUID().toString)
              val taskStart = new bsp4j.TaskStartParams(taskId)
              taskStart.setMessage(s"Packaging $targetName")
              buildClient.onBuildTaskStart(taskStart)

              generator.doPackage(logger, build) match
                case Right(()) =>
                  val taskFinish = new TaskFinishParams(taskId, bsp4j.StatusCode.OK)
                  taskFinish.setMessage(s"Packaged $targetName")
                  buildClient.onBuildTaskFinish(taskFinish)
                case err @ Left(ex) =>
                  val taskFinish = new TaskFinishParams(taskId, bsp4j.StatusCode.ERROR)
                  taskFinish.setMessage(s"error when packaging $targetName: ${ex.getMessage}")
                  buildClient.onBuildTaskFinish(taskFinish)
                  value(err)
          case _ =>
            ()
      }

      val builds = value(buildScopes(b))

      val moduleProjectName = b.cm.platform.map(_ => b.cm.module.projectName)

      ResourceMapper.copyResourceToClassesDir(builds.main, moduleProjectName, configDir)
      for (testBuild <- builds.get(Scope.Test))
        ResourceMapper.copyResourceToClassesDir(testBuild, moduleProjectName, configDir)

      if (actionableDiagnostics.getOrElse(true)) {
        val projectOptions = builds.get(Scope.Test).getOrElse(builds.main).options
        projectOptions.logActionableDiagnostics(logger)
      }

      acc + (b.cm.bloopName -> builds)
    }

    val resultLookup = crossModules.map { cm =>
      cm.module.projectName -> res(cm.bloopName)
    }.groupMap((k, _) => k)((_, v) => v)

    resultLookup
  }

  private def build(
    inputs: Inputs,
    sources: Sources,
    generatedSources: Seq[GeneratedSource],
    options: BuildOptions,
    scope: Scope,
    logger: Logger,
    buildClient: BloopBuildClient,
    compiler: ScalaCompiler,
    buildTests: Boolean,
    partial: Option[Boolean],
    actionableDiagnostics: Option[Boolean],
    moduleProjectName: Option[String],
    optPlatform: Option[Platform],
    dependsOn: List[String],
    workspace: Option[os.Path]
  )(using ScalaCliInvokeData): Either[BuildException, Build] = either {

    val build0 = value {
      buildOnce(
        inputs,
        sources,
        generatedSources,
        options,
        scope,
        logger,
        buildClient,
        compiler,
        partial,
        moduleProjectName = moduleProjectName,
        optPlatform = optPlatform,
        dependsOn = dependsOn,
        workspace = workspace
      )
    }

    build0 match {
      case successful: Successful =>
        if (options.jmhOptions.runJmh.getOrElse(false) && scope == Scope.Main)
          value {
            val res = jmhBuild(
              inputs,
              successful,
              logger,
              successful.options.javaHome().value.javaCommand,
              buildClient,
              compiler,
              buildTests,
              actionableDiagnostics = actionableDiagnostics
            )
            res.flatMap {
              case Some(b) => Right(b)
              case None    => Left(new JmhBuildFailedError)
            }
          }
        else
          build0
      case _ => build0
    }
  }

  def packagedRootDir(root: os.Path, projectName: String): os.Path =
    root / Constants.workspaceDirName / projectName / "bin"
  def resourcesRootDir(root: os.Path, projectName: String): os.Path =
    root / Constants.workspaceDirName / projectName / "managed-resources"
  def classesRootDir(root: os.Path, projectName: String): os.Path =
    root / Constants.workspaceDirName / projectName / "classes"
  def classesDir(
    root: os.Path,
    projectName: String,
    scope: Scope,
    suffix: String = "",
    optPlatform: Option[Platform] = None
  ): os.Path =
    val pre = classesRootDir(root, projectName) / s"${scope.name}$suffix"
    optPlatform match
      case Some(platform) => pre / Platform.normalize(platform.repr)
      case None           => pre

  def resourcesRegistry(
    root: os.Path,
    projectName: String,
    scope: Scope
  ): os.Path =
    root / Constants.workspaceDirName / projectName / s"resources-${scope.name}"

  def scalaNativeSupported(
    options: BuildOptions,
    inputs: Inputs,
    logger: Logger
  ): Either[BuildException, Option[ScalaNativeCompatibilityError]] =
    either {
      val scalaParamsOpt = value(options.scalaParams)
      scalaParamsOpt.flatMap { scalaParams =>
        val scalaVersion       = scalaParams.scalaVersion
        val nativeVersionMaybe = options.scalaNativeOptions.numeralVersion
        def snCompatError =
          Left(
            new ScalaNativeCompatibilityError(
              scalaVersion,
              options.scalaNativeOptions.finalVersion
            )
          )
        def warnIncompatibleNativeOptions(numeralVersion: SNNumeralVersion) =
          if (
            numeralVersion < SNNumeralVersion(0, 4, 4)
            && options.scalaNativeOptions.embedResources.isDefined
          )
            logger.diagnostic(
              "This Scala Version cannot embed resources, regardless of the options used."
            )

        val numeralOrError: Either[ScalaNativeCompatibilityError, SNNumeralVersion] =
          nativeVersionMaybe match {
            case Some(snNumeralVer) =>
              if (snNumeralVer < SNNumeralVersion(0, 4, 1) && Properties.isWin)
                snCompatError
              else if (scalaVersion.startsWith("3.0"))
                snCompatError
              else if (scalaVersion.startsWith("3"))
                if (snNumeralVer >= SNNumeralVersion(0, 4, 3)) Right(snNumeralVer)
                else snCompatError
              else if (scalaVersion.startsWith("2.13"))
                Right(snNumeralVer)
              else if (scalaVersion.startsWith("2.12"))
                if (
                  inputs.sourceFiles().forall {
                    case _: AnyScript => snNumeralVer >= SNNumeralVersion(0, 4, 3)
                    case _            => true
                  }
                ) Right(snNumeralVer)
                else snCompatError
              else snCompatError
            case None => snCompatError
          }

        numeralOrError match {
          case Left(compatError) => Some(compatError)
          case Right(snNumeralVersion) =>
            warnIncompatibleNativeOptions(snNumeralVersion)
            None
        }
      }
    }

  private def inputsToModules(inputs: Inputs): Seq[BuildModule] =
    Seq(BuildModule(inputs, inputs.projectName, Nil, Nil, Nil))

  def build(
    inputs: Inputs,
    options: BuildOptions,
    compilerMaker: ScalaCompilerMaker,
    docCompilerMakerOpt: Option[ScalaCompilerMaker],
    logger: Logger,
    crossBuilds: Boolean,
    buildTests: Boolean,
    partial: Option[Boolean],
    actionableDiagnostics: Option[Boolean]
  )(using ScalaCliInvokeData): Either[BuildException, Builds] = either {
    val modules = inputsToModules(inputs)
    val res = value(build0(
      modules,
      options,
      compilerMaker,
      docCompilerMakerOpt,
      logger,
      crossBuilds,
      buildTests,
      partial,
      actionableDiagnostics,
      configDir = None
    ))
    res.head(1).head // extract the first build
  }

  def build0(
    modules: Seq[BuildModule],
    options: BuildOptions,
    compilerMaker: ScalaCompilerMaker,
    docCompilerMakerOpt: Option[ScalaCompilerMaker],
    logger: Logger,
    crossBuilds: Boolean,
    buildTests: Boolean,
    partial: Option[Boolean],
    actionableDiagnostics: Option[Boolean],
    configDir: Option[os.Path]
  )(using ScalaCliInvokeData): Either[BuildException, Map[String, Seq[Builds]]] = {
    val buildClient = BloopBuildClient.create(
      logger,
      keepDiagnostics = options.internal.keepDiagnostics,
      configDir = configDir
    )

    val workspace = configDir.getOrElse(modules.head.inputs.workspace)

    val classesDir0 = classesRootDir(
      workspace,
      modules.head.projectName
    )

    compilerMaker.withCompiler(
      workspace / Constants.workspaceDirName,
      classesDir0,
      buildClient,
      logger
    ) { compiler =>
      docCompilerMakerOpt match {
        case None =>
          build(
            modules = modules,
            options = options,
            logger = logger,
            buildClient = buildClient,
            compiler = compiler,
            docCompilerOpt = None,
            crossBuilds = crossBuilds,
            buildTests = buildTests,
            partial = partial,
            actionableDiagnostics = actionableDiagnostics,
            configDir = configDir
          )
        case Some(docCompilerMaker) =>
          docCompilerMaker.withCompiler(
            workspace / Constants.workspaceDirName,
            classesDir0, // ???
            buildClient,
            logger
          ) { docCompiler =>
            build(
              modules = modules,
              options = options,
              logger = logger,
              buildClient = buildClient,
              compiler = compiler,
              docCompilerOpt = Some(docCompiler),
              crossBuilds = crossBuilds,
              buildTests = buildTests,
              partial = partial,
              actionableDiagnostics = actionableDiagnostics,
              configDir = configDir
            )
          }
      }
    }
  }

  def validate(
    logger: Logger,
    options: BuildOptions
  ): Either[BuildException, Unit] = {
    val (errors, otherDiagnostics) = options.validate.partition(_.severity == Severity.Error)
    logger.log(otherDiagnostics)
    if (errors.nonEmpty)
      Left(CompositeBuildException(errors.map(new ValidationException(_))))
    else
      Right(())
  }

  def watch(
    inputs: Inputs,
    options: BuildOptions,
    compilerMaker: ScalaCompilerMaker,
    docCompilerMakerOpt: Option[ScalaCompilerMaker],
    logger: Logger,
    crossBuilds: Boolean,
    buildTests: Boolean,
    partial: Option[Boolean],
    actionableDiagnostics: Option[Boolean],
    postAction: () => Unit = () => ()
  )(action: Either[BuildException, Builds] => Unit)(using ScalaCliInvokeData): Watcher = {

    val buildClient = BloopBuildClient.create(
      logger,
      keepDiagnostics = options.internal.keepDiagnostics
    )
    val threads     = BuildThreads.create()
    val classesDir0 = classesRootDir(inputs.workspace, inputs.projectName)
    val compiler = compilerMaker.create(
      inputs.workspace / Constants.workspaceDirName,
      classesDir0,
      buildClient,
      logger
    )
    val docCompilerOpt = docCompilerMakerOpt.map(_.create(
      inputs.workspace / Constants.workspaceDirName,
      classesDir0,
      buildClient,
      logger
    ))

    var res: Either[BuildException, Builds] = null

    def run(): Unit = {
      try {
        res = build(
          inputsToModules(inputs),
          options,
          logger,
          buildClient,
          compiler,
          docCompilerOpt,
          crossBuilds = crossBuilds,
          buildTests = buildTests,
          partial = partial,
          actionableDiagnostics = actionableDiagnostics,
          configDir = None
        ).map(_.head(1).head) // Map("project3282" -> Seq(Builds(...))) => extract the Build
        action(res)
      }
      catch {
        case NonFatal(e) =>
          Util.printException(e)
      }
      postAction()
    }

    run()

    val watcher = new Watcher(ListBuffer(), threads.fileWatcher, run(), compiler.shutdown())

    def doWatch(): Unit = {
      val elements: Seq[Element] =
        if (res == null) inputs.elements
        else
          res
            .map { builds =>
              val mainElems = builds.main.inputs.elements
              val testElems = builds.get(Scope.Test).map(_.inputs.elements).getOrElse(Nil)
              (mainElems ++ testElems).distinct
            }
            .getOrElse(inputs.elements)
      for (elem <- elements) {
        val depth = elem match {
          case _: SingleFile => -1
          case _             => Int.MaxValue
        }
        val eventFilter: PathWatchers.Event => Boolean = elem match {
          case d: Directory =>
            // Filtering event for directories, to ignore those related to the .bloop directory in particular
            event =>
              val p           = os.Path(event.getTypedPath.getPath.toAbsolutePath)
              val relPath     = p.relativeTo(d.path)
              val isHidden    = relPath.segments.exists(_.startsWith("."))
              val pathLast    = relPath.lastOpt.orElse(p.lastOpt).getOrElse("")
              def isScalaFile = pathLast.endsWith(".sc") || pathLast.endsWith(".scala")
              def isJavaFile  = pathLast.endsWith(".java")
              !isHidden && (isScalaFile || isJavaFile)
          case _ => _ => true
        }

        val watcher0 = watcher.newWatcher()
        elem match {
          case d: OnDisk =>
            watcher0.register(d.path.toNIO, depth)
          case _: Virtual =>
        }
        watcher0.addObserver {
          onChangeBufferedObserver { event =>
            if (eventFilter(event))
              watcher.schedule()
          }
        }
      }

      val artifacts = res
        .map { builds =>
          def artifacts(build: Build): Seq[os.Path] =
            build.successfulOpt.toSeq.flatMap(_.artifacts.classPath)
          val main               = artifacts(builds.main)
          val test               = builds.get(Scope.Test).map(artifacts).getOrElse(Nil)
          val allScopesArtifacts = (main ++ test).distinct

          allScopesArtifacts
            .filterNot(_.segments.contains(Constants.workspaceDirName))
        }
        .getOrElse(Nil)
      for (artifact <- artifacts) {
        val depth    = if (os.isFile(artifact)) -1 else Int.MaxValue
        val watcher0 = watcher.newWatcher()
        watcher0.register(artifact.toNIO, depth)
        watcher0.addObserver {
          onChangeBufferedObserver { _ =>
            watcher.schedule()
          }
        }
      }
    }

    try doWatch()
    catch {
      case NonFatal(e) =>
        watcher.dispose()
        throw e
    }

    watcher
  }

  def releaseFlag(
    options: BuildOptions,
    compilerJvmVersionOpt: Option[Positioned[Int]],
    logger: Logger
  ): Option[Int] = {
    lazy val javaHome = options.javaHome()
    if (compilerJvmVersionOpt.exists(javaHome.value.version > _.value)) {
      logger.log(List(Diagnostic(
        Diagnostic.Messages.bloopTooOld,
        Severity.Warning,
        javaHome.positions ++ compilerJvmVersionOpt.map(_.positions).getOrElse(Nil)
      )))
      None
    }
    else if (compilerJvmVersionOpt.exists(_.value == 8))
      None
    else if (
      options.scalaOptions.scalacOptions.values.exists(opt =>
        opt.headOption.exists(_.value.value.startsWith("-release")) ||
        opt.headOption.exists(_.value.value.startsWith("-java-output-version"))
      )
    )
      None
    else if (compilerJvmVersionOpt.isEmpty && javaHome.value.version == 8)
      None
    else
      Some(javaHome.value.version)
  }

  /** Builds a Bloop project.
    *
    * @param inputs
    *   inputs to be included in the project
    * @param sources
    *   sources to be included in the project
    * @param generatedSources
    *   sources generated by Scala CLI as part of the build
    * @param options
    *   build options
    * @param compilerJvmVersionOpt
    *   compiler JVM version (optional)
    * @param scope
    *   build scope for which the project is to be created
    * @param logger
    *   logger
    * @param maybeRecoverOnError
    *   a function handling [[BuildException]] instances, possibly recovering them; returns None on
    *   recovery, Some(e: BuildException) otherwise
    * @return
    *   a bloop [[Project]]
    */
  def buildProject(
    inputs: Inputs,
    sources: Sources,
    generatedSources: Seq[GeneratedSource],
    options: BuildOptions,
    compilerJvmVersionOpt: Option[Positioned[Int]],
    scope: Scope,
    logger: Logger,
    artifacts: Artifacts,
    maybeRecoverOnError: BuildException => Option[BuildException] = e => Some(e),
    moduleProjectName: Option[String] = None,
    optPlatform: Option[Platform] = None,
    dependsOn: List[String] = Nil,
    workspacePath: Option[os.Path] = None
  ): Either[BuildException, Project] = either {

    val allSources = sources.paths.map(_._1) ++ generatedSources.map(_.generated)

    val workspace = workspacePath.getOrElse(inputs.workspace)

    val mainProjectName = moduleProjectName.getOrElse(inputs.projectName)
    val scopedProjectName =
      (moduleProjectName, optPlatform) match
        case (Some(moduleName), Some(platform0)) =>
          val baseName =
            if platform0 == Platform.JVM then moduleName
            else s"$moduleName-${Platform.normalize(platform0.repr)}"
          if scope == Scope.Main then baseName else s"$baseName-${scope.name}"
        case _ => inputs.scopeProjectName(scope)

    val classesDir0 = classesDir(workspace, mainProjectName, scope, optPlatform = optPlatform)
    val scaladocDir =
      classesDir(workspace, mainProjectName, scope, suffix = "-doc", optPlatform = optPlatform)

    val generateSemanticDbs = options.scalaOptions.generateSemanticDbs.getOrElse(false)

    val releaseFlagVersion = releaseFlag(options, compilerJvmVersionOpt, logger).map(_.toString)

    val scalaCompilerParamsOpt = artifacts.scalaOpt match {
      case Some(scalaArtifacts) =>
        val params = value(options.scalaParams).getOrElse {
          sys.error(
            "Should not happen (inconsistency between Scala parameters in BuildOptions and ScalaArtifacts)"
          )
        }

        val pluginScalacOptions = scalaArtifacts.compilerPlugins.distinct.map {
          case (_, _, path) =>
            ScalacOpt(s"-Xplugin:$path")
        }

        val semanticDbScalacOptions =
          if (generateSemanticDbs)
            if (params.scalaVersion.startsWith("2."))
              Seq(
                "-Yrangepos",
                "-P:semanticdb:failures:warning",
                "-P:semanticdb:synthetics:on",
                s"-P:semanticdb:sourceroot:$workspace"
              ).map(ScalacOpt(_))
            else
              Seq(
                "-Xsemanticdb",
                "-sourceroot",
                workspace.toString
              ).map(ScalacOpt(_))
          else Nil

        val sourceRootScalacOptions =
          if (params.scalaVersion.startsWith("2.")) Nil
          else Seq("-sourceroot", workspace.toString).map(ScalacOpt(_))

        val scalaJsScalacOptions =
          if (options.platform.value == Platform.JS && !params.scalaVersion.startsWith("2."))
            Seq(ScalacOpt("-scalajs"))
          else Nil

        val scalacReleaseV =
          // the -release flag is not supported for Scala 2.12.x < 2.12.5
          if params.scalaVersion.asVersion < "2.12.5".asVersion then Nil
          else
            releaseFlagVersion
              .map(v => List("-release", v).map(ScalacOpt(_)))
              .getOrElse(Nil)

        val scalapyOptions =
          if (
            params.scalaVersion.startsWith("2.13.") &&
            options.notForBloopOptions.python.getOrElse(false)
          )
            Seq(ScalacOpt("-Yimports:java.lang,scala,scala.Predef,me.shadaj.scalapy"))
          else Nil

        val scalacOptions =
          options.scalaOptions.scalacOptions.map(_.value) ++
            pluginScalacOptions ++
            semanticDbScalacOptions ++
            sourceRootScalacOptions ++
            scalaJsScalacOptions ++
            scalacReleaseV ++
            scalapyOptions

        val compilerParams = ScalaCompilerParams(
          scalaVersion = params.scalaVersion,
          scalaBinaryVersion = params.scalaBinaryVersion,
          scalacOptions = scalacOptions.toSeq.map(_.value),
          compilerClassPath = scalaArtifacts.compilerClassPath
        )
        Some(compilerParams)

      case None =>
        None
    }

    val javacOptions = {

      val semanticDbJavacOptions =
        // FIXME Should this be in scalaOptions, now that we use it for javac stuff too?
        if (generateSemanticDbs) {
          // from https://github.com/scalameta/metals/blob/04405c0401121b372ea1971c361e05108fb36193/metals/src/main/scala/scala/meta/internal/metals/JavaInteractiveSemanticdb.scala#L137-L146
          val compilerPackages = Seq(
            "com.sun.tools.javac.api",
            "com.sun.tools.javac.code",
            "com.sun.tools.javac.model",
            "com.sun.tools.javac.tree",
            "com.sun.tools.javac.util"
          )
          val exports = compilerPackages.flatMap { pkg =>
            Seq("-J--add-exports", s"-Jjdk.compiler/$pkg=ALL-UNNAMED")
          }

          Seq(
            // does the path need to be escaped somehow?
            s"-Xplugin:semanticdb -sourceroot:$workspace -targetroot:javac-classes-directory"
          ) ++ exports
        }
        else
          Nil

      val javacReleaseV = releaseFlagVersion.map(v => List("--release", v)).getOrElse(Nil)

      javacReleaseV ++ semanticDbJavacOptions ++ options.javaOptions.javacOptions.map(_.value)
    }

    // `test` scope should contains class path to main scope
    val mainClassesPath =
      if (scope == Scope.Test)
        List(classesDir(workspace, mainProjectName, Scope.Main, optPlatform = optPlatform))
      else Nil

    value(validate(logger, options))

    val fullClassPath = artifacts.compileClassPath ++
      mainClassesPath ++
      artifacts.javacPluginDependencies.map(_._3) ++
      artifacts.extraJavacPlugins

    val project = Project(
      directory = workspace / Constants.workspaceDirName,
      workspace = workspace,
      classesDir = classesDir0,
      scaladocDir = scaladocDir,
      scalaCompiler = scalaCompilerParamsOpt,
      scalaJsOptions =
        if (options.platform.value == Platform.JS) Some(options.scalaJsOptions.config(logger))
        else None,
      scalaNativeOptions =
        if (options.platform.value == Platform.Native)
          Some(options.scalaNativeOptions.bloopConfig())
        else None,
      projectName = scopedProjectName,
      classPath = fullClassPath,
      resolution = Some(Project.resolution(artifacts.detailedArtifacts)),
      sources = allSources,
      resourceDirs = sources.resourceDirs,
      scope = scope,
      javaHomeOpt = Option(options.javaHomeLocation().value),
      javacOptions = javacOptions,
      dependsOn = dependsOn
    )
    project
  }

  def prepareBuild(
    inputs: Inputs,
    sources: Sources,
    generatedSources: Seq[GeneratedSource],
    options: BuildOptions,
    compilerJvmVersionOpt: Option[Positioned[Int]],
    scope: Scope,
    compiler: ScalaCompiler,
    logger: Logger,
    buildClient: BloopBuildClient,
    maybeRecoverOnError: BuildException => Option[BuildException] = e => Some(e),
    moduleProjectName: Option[String] = None,
    optPlatform: Option[Platform] = None,
    dependsOn: List[String] = Nil,
    workspace: Option[os.Path] = None
  ): Either[BuildException, (os.Path, Option[ScalaParameters], Artifacts, Project, Boolean)] =
    either {

      val options0 =
        if (sources.hasJava && !sources.hasScala)
          options.copy(
            scalaOptions = options.scalaOptions.copy(
              scalaVersion = options.scalaOptions.scalaVersion.orElse {
                Some(MaybeScalaVersion.none)
              }
            )
          )
        else
          options
      val params = value(options0.scalaParams)

      val scopeParams =
        if (scope == Scope.Main) Nil
        else Seq(scope.name)

      buildClient.setProjectParams(scopeParams ++ value(options0.projectParams))

      val classesDir0 = (workspace, moduleProjectName, optPlatform) match
        case (Some(configDir), Some(projectN), platform0 @ Some(_)) =>
          classesDir(configDir, projectN, scope, optPlatform = platform0)
        case _ =>
          classesDir(inputs.workspace, inputs.projectName, scope)

      val artifacts = value(options0.artifacts(logger, scope, maybeRecoverOnError))

      value(validate(logger, options0))

      val project = value {
        buildProject(
          inputs,
          sources,
          generatedSources,
          options0,
          compilerJvmVersionOpt,
          scope,
          logger,
          artifacts,
          maybeRecoverOnError,
          moduleProjectName = moduleProjectName,
          optPlatform = optPlatform,
          dependsOn,
          workspace
        )
      }

      val projectChanged = compiler.prepareProject(project, logger)

      if (compiler.usesClassDir && projectChanged && os.isDir(classesDir0)) {
        logger.debug(s"Clearing $classesDir0")
        os.list(classesDir0).foreach { p =>
          logger.debug(s"Removing $p")
          try os.remove.all(p)
          catch {
            case ex: FileSystemException =>
              logger.debug(s"Ignoring $ex while cleaning up $p")
          }
        }
      }

      (classesDir0, params, artifacts, project, projectChanged)
    }

  def buildOnce(
    inputs: Inputs,
    sources: Sources,
    generatedSources: Seq[GeneratedSource],
    options: BuildOptions,
    scope: Scope,
    logger: Logger,
    buildClient: BloopBuildClient,
    compiler: ScalaCompiler,
    partialOpt: Option[Boolean],
    moduleProjectName: Option[String] = None,
    optPlatform: Option[Platform] = None,
    dependsOn: List[String] = Nil,
    workspace: Option[os.Path] = None
  ): Either[BuildException, Build] = either {

    if (options.platform.value == Platform.Native)
      value(scalaNativeSupported(options, inputs, logger)) match {
        case None        =>
        case Some(error) => value(Left(error))
      }

    val (classesDir0, scalaParams, artifacts, project, projectChanged) = value {
      prepareBuild(
        inputs,
        sources,
        generatedSources,
        options,
        compiler.jvmVersion,
        scope,
        compiler,
        logger,
        buildClient,
        dependsOn = dependsOn,
        moduleProjectName = moduleProjectName,
        optPlatform = optPlatform,
        workspace = workspace
      )
    }

    if (compiler.usesClassDir && projectChanged && os.isDir(classesDir0)) {
      logger.debug(s"Clearing $classesDir0")
      os.list(classesDir0).foreach { p =>
        logger.debug(s"Removing $p")
        try os.remove.all(p)
        catch {
          case ex: FileSystemException =>
            logger.debug(s"Ignore $ex while removing $p")
        }
      }
    }

    buildClient.clear()
    buildClient.setGeneratedSources(scope, generatedSources)

    val partial = partialOpt.getOrElse {
      options.notForBloopOptions.packageOptions.packageTypeOpt.exists(_.sourceBased)
    }

    val success = partial || compiler.compile(project, logger)

    if (success)
      val refreshed = buildClient.compileTasks.exists(_.exists(_ == project.projectName))
      Successful(
        inputs,
        options,
        scalaParams,
        scope,
        sources,
        artifacts,
        project,
        classesDir0,
        buildClient.diagnostics,
        generatedSources,
        partial,
        refreshed
      )
    else
      Failed(
        inputs,
        options,
        scope,
        sources,
        artifacts,
        project,
        buildClient.diagnostics
      )
  }

  def postProcess(
    generatedSources: Seq[GeneratedSource],
    generatedSrcRoot: os.Path,
    classesDir: os.Path,
    logger: Logger,
    workspace: os.Path,
    updateSemanticDbs: Boolean,
    scalaVersion: String
  ): Either[Seq[String], Unit] =
    if (os.exists(classesDir)) {

      // TODO Write classes to a separate directory during post-processing
      logger.debug("Post-processing class files of pre-processed sources")
      val mappings = generatedSources
        .map { source =>
          val relPath       = source.generated.relativeTo(generatedSrcRoot).toString
          val reportingPath = source.reportingPath.fold(s => s, _.last)
          (relPath, (reportingPath, scalaLineToScLineShift(source.wrapperParamsOpt)))
        }
        .toMap

      val postProcessors =
        Seq(ByteCodePostProcessor) ++
          (if (updateSemanticDbs) Seq(SemanticDbPostProcessor) else Nil) ++
          Seq(TastyPostProcessor)

      val failures = postProcessors.flatMap(
        _.postProcess(generatedSources, mappings, workspace, classesDir, logger, scalaVersion)
          .fold(e => Seq(e), _ => Nil)
      )
      if (failures.isEmpty) Right(()) else Left(failures)
    }
    else
      Right(())

  def onChangeBufferedObserver(onEvent: PathWatchers.Event => Unit): Observer[PathWatchers.Event] =
    new Observer[PathWatchers.Event] {
      def onError(t: Throwable): Unit = {
        // TODO Log that properly
        System.err.println("got error:")
        @tailrec
        def printEx(t: Throwable): Unit =
          if (t != null) {
            System.err.println(t)
            System.err.println(
              t.getStackTrace.iterator.map("  " + _ + System.lineSeparator()).mkString
            )
            printEx(t.getCause)
          }
        printEx(t)
      }

      def onNext(event: PathWatchers.Event): Unit =
        onEvent(event)
    }

  final class Watcher(
    val watchers: ListBuffer[PathWatcher[PathWatchers.Event]],
    val scheduler: ScheduledExecutorService,
    onChange: => Unit,
    onDispose: => Unit
  ) {
    def newWatcher(): PathWatcher[PathWatchers.Event] = {
      val w = PathWatchers.get(true)
      watchers += w
      w
    }
    def dispose(): Unit = {
      onDispose
      watchers.foreach(_.close())
      scheduler.shutdown()
    }

    private val lock                  = new Object
    private var f: ScheduledFuture[?] = _
    private val waitFor               = 50.millis
    private val runnable: Runnable = { () =>
      lock.synchronized {
        f = null
      }
      onChange // FIXME Log exceptions
    }
    def schedule(): Unit =
      if (f == null)
        lock.synchronized {
          if (f == null)
            f = scheduler.schedule(runnable, waitFor.length, waitFor.unit)
        }
  }

  private def printable(path: os.Path): String =
    if (path.startsWith(os.pwd)) path.relativeTo(os.pwd).toString
    else path.toString

  private def jmhBuild(
    inputs: Inputs,
    build: Build.Successful,
    logger: Logger,
    javaCommand: String,
    buildClient: BloopBuildClient,
    compiler: ScalaCompiler,
    buildTests: Boolean,
    actionableDiagnostics: Option[Boolean]
  )(using ScalaCliInvokeData): Either[BuildException, Option[Build]] = either {
    val jmhProjectName = inputs.projectName + "_jmh"
    val jmhOutputDir   = inputs.workspace / Constants.workspaceDirName / jmhProjectName
    os.remove.all(jmhOutputDir)
    val jmhSourceDir   = jmhOutputDir / "sources"
    val jmhResourceDir = jmhOutputDir / "resources"

    val retCode = run(
      javaCommand,
      build.fullClassPath.map(_.toIO),
      "org.openjdk.jmh.generators.bytecode.JmhBytecodeGenerator",
      Seq(printable(build.output), printable(jmhSourceDir), printable(jmhResourceDir), "default"),
      logger
    )
    if (retCode != 0) {
      val red      = Console.RED
      val lightRed = "\u001b[91m"
      val reset    = Console.RESET
      System.err.println(
        s"${red}jmh bytecode generator exited with return code $lightRed$retCode$red.$reset"
      )
    }

    if (retCode == 0) {
      val jmhInputs = inputs.copy(
        baseProjectName = jmhProjectName,
        // hash of the underlying project if needed is already in jmhProjectName
        mayAppendHash = false,
        elements = inputs.elements ++ Seq(
          Directory(jmhSourceDir),
          ResourceDirectory(jmhResourceDir)
        )
      )
      val updatedOptions = build.options.copy(
        jmhOptions = build.options.jmhOptions.copy(
          runJmh = build.options.jmhOptions.runJmh.map(_ => false)
        )
      )
      val jmhBuilds = value {
        Build.build(
          inputsToModules(jmhInputs),
          updatedOptions,
          logger,
          buildClient,
          compiler,
          None,
          crossBuilds = false,
          buildTests = buildTests,
          partial = None,
          actionableDiagnostics = actionableDiagnostics,
          configDir = None
        )
      }.head(1).head // extract the first build from the map
      Some(jmhBuilds.main)
    }
    else None
  }

  private def run(
    javaCommand: String,
    classPath: Seq[File],
    mainClass: String,
    args: Seq[String],
    logger: Logger
  ): Int = {

    val command =
      Seq(javaCommand) ++
        Seq(
          "-cp",
          classPath.iterator.map(_.getAbsolutePath).mkString(File.pathSeparator),
          mainClass
        ) ++
        args

    logger.log(
      s"Running ${command.mkString(" ")}",
      "  Running" + System.lineSeparator() +
        command.iterator.map(_ + System.lineSeparator()).mkString
    )

    new ProcessBuilder(command*)
      .inheritIO()
      .start()
      .waitFor()
  }

  trait ModuleLike[T] {
    def id(t: T): String

    // def weight(t: T): Int

    def dependsOn(t: T): Seq[String]
  }

  object ModuleLike {
    inline def apply[T](using m: ModuleLike[T]): m.type = m
  }

  object Dag {
    def topologicalSort[T: ModuleLike](modules: Seq[T]): Seq[Seq[T]] = {
      import scala.collection.mutable
      import scala.annotation.tailrec

      val byId   = modules.map(m => ModuleLike[T].id(m) -> m).toMap
      val lookup = byId.map((id, m) => id -> ModuleLike[T].dependsOn(m))

      val outgoingEdges =
        lookup.map((k, v) => k -> v.to(mutable.LinkedHashSet))

      val incomingEdges: mutable.SeqMap[String, mutable.LinkedHashSet[String]] =
        val buf = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[String]]
        for (k, v) <- lookup do
          buf.getOrElseUpdate(k, mutable.LinkedHashSet.empty[String])
          for dep <- v do
            buf.getOrElseUpdate(dep, mutable.LinkedHashSet.empty[String]).addOne(k)
        buf

      val sNext = mutable.ListBuffer.empty[String]

      @tailrec
      def iterate(s1: List[String], acc: List[List[String]]): List[List[T]] =
        sNext.clear()

        for target <- s1 do
          val outgoing = outgoingEdges(target)
          for dep <- outgoing.toList do
            val incoming = incomingEdges(dep)
            outgoing -= dep
            incoming -= target
            if incoming.isEmpty then
              sNext += dep

        if sNext.isEmpty then
          acc.map(_.map(byId(_)))
        else
          val s2 = sNext.toList
          iterate(s2, s2 :: acc)
      end iterate

      val s0 =
        incomingEdges.collect { case (target, incoming) if incoming.isEmpty => target }.toList
      iterate(s0, s0 :: Nil)
    }
  }
}
