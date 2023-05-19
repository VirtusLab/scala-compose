package scala.build.bsp

import bloop.rifle.{BloopRifleConfig, BloopServer}
import ch.epfl.scala.bsp4j as b
import com.github.plokhotnyuk.jsoniter_scala.core.{
  JsonReaderException,
  readFromArray,
  writeToStringReentrant
}
import dependency.ScalaParameters
import org.eclipse.lsp4j.jsonrpc
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError

import java.io.{InputStream, OutputStream}
import java.util.UUID
import java.util.concurrent.{CompletableFuture, Executor}
import scala.build.EitherCps.{either, value}
import scala.build.*
import scala.build.compiler.BloopCompiler
import scala.build.errors.{
  BuildException,
  CompositeBuildException,
  Diagnostic,
  ParsingInputsException
}
import scala.build.input.{Inputs, ScalaCliInvokeData}
import scala.build.internal.{Constants, CustomCodeWrapper}
import scala.build.options.{BuildOptions, ClassPathOptions, Platform, ScalaOptions, Scope}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success}

/** The implementation for [[Bsp]].
  *
  * @param argsToInputs
  *   a function transforming terminal args to [[Inputs]]
  * @param bspReloadableOptionsReference
  *   reference to the current instance of [[BspReloadableOptions]]
  * @param threads
  *   BSP threads
  * @param in
  *   the input stream of bytes
  * @param out
  *   the output stream of bytes
  */
final class BspImpl(
  argsToInputs: Seq[String] => Either[BuildException, Seq[Module]],
  bspReloadableOptionsReference: BspReloadableOptions.Reference,
  threads: BspThreads,
  in: InputStream,
  out: OutputStream,
  actionableDiagnostics: Option[Boolean],
  configDir: Option[os.Path]
)(using ScalaCliInvokeData) extends Bsp {

  import BspImpl.{PreBuildData, PreBuildProject, buildTargetIdToEvent, responseError}

  /** Sends the buildTarget/didChange BSP notification to the BSP client, indicating that the build
    * targets defined in the current session have changed.
    *
    * @param currentBloopSession
    *   the current Bloop session
    */
  private def notifyBuildChange(currentBloopSession: BloopSession): Unit = {
    val events =
      for (targetId <- currentBloopSession.bspServer.targetIds)
        yield {
          val event = new b.BuildTargetEvent(targetId)
          event.setKind(b.BuildTargetEventKind.CHANGED)
          event
        }
    val params = new b.DidChangeBuildTarget(events.asJava)
    actualLocalClient.onBuildTargetDidChange(params)
  }

  /** Initial setup for the Bloop project.
    *
    * @param currentBloopSession
    *   the current Bloop session
    * @param reloadableOptions
    *   options which may be reloaded on a bsp workspace/reload request
    * @param maybeRecoverOnError
    *   a function handling [[BuildException]] instances based on [[Scope]], possibly recovering
    *   them; returns None on recovery, Some(e: BuildException) otherwise
    */
  private def prepareBuild(
    currentBloopSession: BloopSession,
    reloadableOptions: BspReloadableOptions,
    maybeRecoverOnError: Scope => BuildException => Option[BuildException] = _ => e => Some(e)
  ): Either[(BuildException, Scope), Seq[PreBuildProject]] = either[(BuildException, Scope)] {
    val logger       = reloadableOptions.logger
    val buildOptions = reloadableOptions.buildOptions
    val verbosity    = reloadableOptions.verbosity
    logger.log("Preparing build")

    val persistentLogger = new PersistentDiagnosticLogger(logger)
    val bspServer        = currentBloopSession.bspServer

    type MultiModule = CrossModule

    val crossModules: Seq[MultiModule] = currentBloopSession.modules.flatMap { module =>
      val inputs = module.inputs

      // allInputs contains elements from using directives
      val (crossSources, allInputs) = value {
        CrossSources.forInputs(
          inputs = inputs,
          preprocessors = Sources.defaultPreprocessors(
            buildOptions.archiveCache,
            buildOptions.internal.javaClassNameVersionOpt,
            () => buildOptions.javaHome().value.javaCommand
          ),
          logger = persistentLogger,
          suppressWarningOptions = buildOptions.suppressWarningOptions,
          exclude = buildOptions.internal.exclude,
          maybeRecoverOnError = maybeRecoverOnError(Scope.Main)
        ).left.map((_, Scope.Main))
      }

    val sharedOptions = crossSources.sharedOptions(buildOptions)

    if (verbosity >= 3)
      pprint.err.log(crossSources)

    val scopedSources =
      value(crossSources.scopedSources(buildOptions).left.map((_, Scope.Main)))

      if (verbosity >= 3)
        pprint.err.log(scopedSources)

      val mainModule = CrossModule(module, allInputs, crossSources, scopedSources)
      mainModule :: Nil
    }

    val crossModuleLookup = crossModules.map(cm => cm.module.projectName -> cm).toMap

    extension (cm: CrossModule)
      def resourcesDir: os.Path =
        configDir match
          case Some(workspace) =>
            Build.resourcesRootDir(workspace, cm.module.projectName)
          case None =>
            Build.resourcesRootDir(
              cm.allInputs.workspace,
              cm.allInputs.projectName
            )

      def scopedClassesDir(scope: Scope): os.Path =
        configDir match
          case Some(workspace) =>
            Build.classesDir(workspace, cm.module.projectName, scope)
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
      ): BuildOptions =
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

        def step(acc: BuildOptions, dep: String) =
          val depM = crossModuleLookup(dep)
          val classesDir0 =
            if scope == Scope.Test then None
            else Some(depM.scopedClassesDir(scope))
          val sharedOpts = getDepOptions(depM.crossSources.sharedOptions(acc), classesDir0)
          val acc1 =
            depM
              .scopedSources
              .buildOptions
              .flatMap(_.valueFor(scope))
              .foldRight(sharedOpts)((d, acc) => getDepOptions(d, None).orElse(acc))
          depM.depOptions(acc1, scope, platform)

        cm.module.dependsOn.foldLeft(baseOptions)(step)
      end depOptions

    crossModules.map { cm =>

      val sharedOptions = cm.crossSources.sharedOptions(buildOptions)

      val platformArg =
        if cm.module.platforms.sizeIs == 1 then
          cm.module.platforms.head match
            case "jvm"          => Some(scala.build.options.Platform.JVM)
            case "scala-js"     => Some(scala.build.options.Platform.JS)
            case "scala-native" => Some(scala.build.options.Platform.Native)
        else
          None

      val platformArg0 =
        platformArg.map(p => Positioned(List(Position.Custom("DEFAULT-COMPOSE")), p))

      val modulePlatform = platformArg0.getOrElse(sharedOptions.platform)

      val mainDeps = cm.depOptions(buildOptions, Scope.Main, modulePlatform)
      val testDeps = cm.depOptions(buildOptions, Scope.Test, modulePlatform)

      val sourcesMain0 = value {
        cm.scopedSources.sources(Scope.Main, sharedOptions orElse mainDeps, allInputs.workspace)
          .left.map((_, Scope.Main))
      }

      val sourcesTest0 = value {
        cm.scopedSources.sources(Scope.Test, sharedOptions orElse testDeps, allInputs.workspace)
          .left.map((_, Scope.Test))
      }
      def addResource(sources: Sources, resourceDir: os.Path): Sources =
        sources.copy(resourceDirs = resourceDir +: sources.resourceDirs)

      val (sourcesMain, sourcesTest) =
        if cm.module.resourceGenerators.isEmpty then
          (sourcesMain0, sourcesTest0)
        else
          val resourceDir = cm.resourcesDir
          (addResource(sourcesMain0, resourceDir), addResource(sourcesTest0, resourceDir))

      if (verbosity >= 3)
        pprint.err.log(sourcesMain)

      val options0Main = sourcesMain.buildOptions
      val options0Test = sourcesTest.buildOptions.orElse(options0Main)

      val generatedSourcesMain =
        sourcesMain.generateSources(cm.allInputs.generatedSrcRoot(Scope.Main))
      val generatedSourcesTest =
        sourcesTest.generateSources(cm.allInputs.generatedSrcRoot(Scope.Test))

      val resourceDependencies = cm.module.resourceGenerators.map(_(0))

      val mainDependsOn = cm.module.dependsOn ++ resourceDependencies
      val testDependsOn = Nil // will be patched to depend on main

      bspServer.setExtraDependencySources(options0Main.classPathOptions.extraSourceJars)
      bspServer.setExtraTestDependencySources(options0Test.classPathOptions.extraSourceJars)
      bspServer.setGeneratedSources(Scope.Main, generatedSourcesMain)
      bspServer.setGeneratedSources(Scope.Test, generatedSourcesTest)

      val (classesDir0Main, scalaParamsMain, artifactsMain, projectMain, buildChangedMain) = value {
        val res = Build.prepareBuild(
          cm.allInputs,
          sourcesMain,
          generatedSourcesMain,
          options0Main,
          None,
          Scope.Main,
          currentBloopSession.remoteServer,
          persistentLogger,
          localClient,
          maybeRecoverOnError(Scope.Main),
          moduleProjectName = configDir.map(_ => cm.module.projectName),
          dependsOn = mainDependsOn,
          // resourceGenerators = cm.module.resourceGenerators,
          workspace = configDir
        )
        res.left.map((_, Scope.Main))
      }

      val (classesDir0Test, scalaParamsTest, artifactsTest, projectTest, buildChangedTest) = value {
        val res = Build.prepareBuild(
          cm.allInputs,
          sourcesTest,
          generatedSourcesTest,
          options0Test,
          None,
          Scope.Test,
          currentBloopSession.remoteServer,
          persistentLogger,
          localClient,
          maybeRecoverOnError(Scope.Test),
          moduleProjectName = configDir.map(_ => cm.module.projectName),
          dependsOn = testDependsOn,
          // resourceGenerators = Nil,
          workspace = configDir
        )
        res.left.map((_, Scope.Test))
      }

      localClient.setGeneratedSources(Scope.Main, generatedSourcesMain)
      localClient.setGeneratedSources(Scope.Test, generatedSourcesTest)

      val mainScope = PreBuildData(
        sourcesMain,
        options0Main,
        classesDir0Main,
        scalaParamsMain,
        artifactsMain,
        projectMain,
        generatedSourcesMain,
        buildChangedMain,
        mainDependsOn
      )

      val testScope = PreBuildData(
        sourcesTest,
        options0Test,
        classesDir0Test,
        scalaParamsTest,
        artifactsTest,
        projectTest,
        generatedSourcesTest,
        buildChangedTest,
        testDependsOn
      )

      if (actionableDiagnostics.getOrElse(true)) {
        val projectOptions = options0Test.orElse(options0Main)
        projectOptions.logActionableDiagnostics(persistentLogger)
      }

      PreBuildProject(mainScope, testScope, cm.module, persistentLogger.diagnostics)
    }
  }

  private def buildE(
    currentBloopSession: BloopSession,
    notifyChanges: Boolean,
    reloadableOptions: BspReloadableOptions
  ): Either[(BuildException, Scope), Unit] = {

    def doBuildOnce(
      module: Module,
      data: PreBuildData,
      scope: Scope
    ): Either[(BuildException, Scope), Build] =
      Build.buildOnce(
        module.inputs,
        data.sources,
        data.generatedSources,
        data.buildOptions,
        scope,
        reloadableOptions.logger,
        actualLocalClient,
        currentBloopSession.remoteServer,
        partialOpt = None,
        moduleProjectName = configDir.map(_ => module.projectName),
        dependsOn = data.dependsOn,
        workspace = configDir
      ).left.map(_ -> scope)

    either[(BuildException, Scope)] {
      val preBuilds: Seq[PreBuildProject] =
        value(prepareBuild(currentBloopSession, reloadableOptions))

      val sorted = BspImpl.Dag.topologicalSort(preBuilds).flatten // TODO: parallelize levels

      sorted.foldLeft(Map.empty[String, (Build, Boolean)]) { (acc, preBuild) =>
        val mainChanged = preBuild.mainScope.buildChanged
        if (notifyChanges && (mainChanged || preBuild.testScope.buildChanged))
          notifyBuildChange(currentBloopSession)
        preBuild.module.resourceGenerators.foreach((module, doPackage) =>
          acc(module) match
            case (build: Build.Successful, true) => // module has changed and was successful
              value(doPackage(reloadableOptions, build).left.map(_ -> Scope.Main))
            case _ =>
              ()
        )

        val mainBuild = value(doBuildOnce(preBuild.module, preBuild.mainScope, Scope.Main))
        value(doBuildOnce(preBuild.module, preBuild.testScope, Scope.Test))
        acc + (preBuild.module.projectName -> (mainBuild, mainChanged))
      }
      ()
    }
  }

  private def build(
    currentBloopSession: BloopSession,
    client: BspClient,
    notifyChanges: Boolean,
    reloadableOptions: BspReloadableOptions
  ): Unit =
    buildE(currentBloopSession, notifyChanges, reloadableOptions) match {
      case Left((ex, scope)) =>
        currentBloopSession.bspServer.targetScopeIdOpt(scope).foreach { buildTargetId =>
          client.reportBuildException(
            Some(buildTargetId),
            ex
          )
        }
        reloadableOptions.logger.debug(s"Caught $ex during BSP build, ignoring it")
      case Right(()) =>
        for (targetId <- currentBloopSession.bspServer.targetIds)
          client.resetBuildExceptionDiagnostics(targetId)
    }

  private val shownGlobalMessages =
    new java.util.concurrent.ConcurrentHashMap[String, Unit]()

  private def showGlobalWarningOnce(msg: String): Unit =
    shownGlobalMessages.computeIfAbsent(
      msg,
      _ => {
        val params = new b.ShowMessageParams(b.MessageType.WARNING, msg)
        actualLocalClient.onBuildShowMessage(params)
      }
    )

  /** Compilation logic, to be called on a buildTarget/compile BSP request.
    *
    * @param currentBloopSession
    *   the current Bloop session
    * @param executor
    *   executor
    * @param reloadableOptions
    *   options which may be reloaded on a bsp workspace/reload request
    * @param doCompile
    *   (self-)reference to calling the compilation logic
    * @return
    *   a future of [[b.CompileResult]]
    */
  private def compile(
    buildTargetIds: collection.Seq[b.BuildTargetIdentifier],
    currentBloopSession: BloopSession,
    executor: Executor,
    reloadableOptions: BspReloadableOptions,
    doCompile: () => CompletableFuture[b.CompileResult]
  ): CompletableFuture[b.CompileResult] = {
    val preBuild = CompletableFuture.supplyAsync(
      () =>
        prepareBuild(currentBloopSession, reloadableOptions) match {
          case Right(preBuilds) =>
            val someChange = preBuilds.exists(preBuild =>
              preBuild.mainScope.buildChanged || preBuild.testScope.buildChanged
            )
            if (someChange)
              notifyBuildChange(currentBloopSession)
            Right(preBuilds)
          case Left((ex, scope)) =>
            Left((ex, scope))
        },
      executor
    )

    preBuild.thenCompose {
      case Left((ex, scope)) =>
        val taskId = new b.TaskId(UUID.randomUUID().toString)

        for targetId <- currentBloopSession.bspServer.targetScopeIdOpt(scope) do {
          val target = targetId.getUri match {
            case s"$_?id=$targetId" => targetId
            case targetIdUri        => targetIdUri
          }

          val taskStartParams = new b.TaskStartParams(taskId)
          taskStartParams.setEventTime(System.currentTimeMillis())
          taskStartParams.setMessage(s"Preprocessing '$target'")
          taskStartParams.setDataKind(b.TaskStartDataKind.COMPILE_TASK)
          taskStartParams.setData(new b.CompileTask(targetId))

          actualLocalClient.onBuildTaskStart(taskStartParams)

          actualLocalClient.reportBuildException(
            Some(targetId),
            ex
          )

          val taskFinishParams = new b.TaskFinishParams(taskId, b.StatusCode.ERROR)
          taskFinishParams.setEventTime(System.currentTimeMillis())
          taskFinishParams.setMessage(s"Preprocessed '$target'")
          taskFinishParams.setDataKind(b.TaskFinishDataKind.COMPILE_REPORT)

          val errorSize = ex match {
            case c: CompositeBuildException => c.exceptions.size
            case _                          => 1
          }

          taskFinishParams.setData(new b.CompileReport(targetId, errorSize, 0))

          actualLocalClient.onBuildTaskFinish(taskFinishParams)
        }

        CompletableFuture.completedFuture(
          new b.CompileResult(b.StatusCode.ERROR)
        )
      case Right(params) =>
        for (targetId <- currentBloopSession.bspServer.targetIds)
          actualLocalClient.resetBuildExceptionDiagnostics(targetId)

        val mainTargetIds =
          currentBloopSession.bspServer.projectScopeNames(Scope.Main).toMap

        params.foreach { param =>
          val targetId = mainTargetIds(param.mainScope.project.projectName)
          actualLocalClient.reportDiagnosticsForFiles(targetId, param.diagnostics, reset = false)
        }

        // TODO: inspect buildTargetIds and intercept build targets that need resource generators
        doCompile().thenCompose { res =>

          def doPostProcess(data: PreBuildData, scope: Scope): Unit =

            val module = configDir match
              case Some(_) => // TODO: if scala-compose
                val moduleName =
                  if scope == Scope.Test then data.project.projectName.stripSuffix("-test")
                  else data.project.projectName
                currentBloopSession.modules.find(_.projectName == moduleName).get
              case None => currentBloopSession.modules.head

            for (sv <- data.project.scalaCompiler.map(_.scalaVersion))
              Build.postProcess(
                data.generatedSources,
                module.inputs.generatedSrcRoot(scope),
                data.classesDir,
                reloadableOptions.logger,
                configDir.getOrElse(module.inputs.workspace),
                updateSemanticDbs = true,
                scalaVersion = sv
              ).left.foreach(_.foreach(showGlobalWarningOnce))

          if (res.getStatusCode == b.StatusCode.OK)
            CompletableFuture.supplyAsync(
              () =>
                params.foreach(param =>
                  doPostProcess(param.mainScope, Scope.Main)
                  doPostProcess(param.testScope, Scope.Test)
                )
                res
              ,
              executor
            )
          else
            CompletableFuture.completedFuture(res)
        }
    }
  }

  private var actualLocalClient: BspClient                     = _
  private var localClient: b.BuildClient with BloopBuildClient = _

  /** Returns a reference to the [[BspClient]], respecting the given verbosity
    * @param verbosity
    *   verbosity to be passed to the resulting [[BspImpl.LoggingBspClient]]
    * @return
    *   BSP client
    */
  private def getLocalClient(verbosity: Int): b.BuildClient with BloopBuildClient =
    if (verbosity >= 3)
      new BspImpl.LoggingBspClient(actualLocalClient)
    else
      actualLocalClient

  /** Creates a fresh Bloop session
    * @param inputs
    *   all the inputs to be included in the session's context
    * @param reloadableOptions
    *   options which may be reloaded on a bsp workspace/reload request
    * @param presetIntelliJ
    *   a flag marking if this is in context of a BSP connection with IntelliJ (allowing to pass
    *   this setting from a past session)
    * @return
    *   a new [[BloopSession]]
    */
  private def newBloopSession(
    inputs: Seq[Module],
    reloadableOptions: BspReloadableOptions,
    presetIntelliJ: Boolean = false
  ): BloopSession = {
    val logger       = reloadableOptions.logger
    val buildOptions = reloadableOptions.buildOptions
    val createBloopServer =
      () =>
        BloopServer.buildServer(
          reloadableOptions.bloopRifleConfig,
          "scala-cli",
          Constants.version,
          (configDir.getOrElse(inputs.head.inputs.workspace) / Constants.workspaceDirName).toNIO,
          Build.classesRootDir(
            configDir.getOrElse(inputs.head.inputs.workspace),
            inputs.head.projectName
          ).toNIO,
          localClient,
          threads.buildThreads.bloop,
          logger.bloopRifleLogger
        )
    val remoteServer = new BloopCompiler(
      createBloopServer,
      20.seconds,
      strictBloopJsonCheck = buildOptions.internal.strictBloopJsonCheckOrDefault
    )
    lazy val bspServer = new BspServer(
      remoteServer.bloopServer.server,
      (params, doCompile) =>
        compile(params, bloopSession0, threads.prepareBuildExecutor, reloadableOptions, doCompile),
      logger,
      presetIntelliJ
    )

    lazy val watcher = new Build.Watcher(
      ListBuffer(),
      threads.buildThreads.fileWatcher,
      build(bloopSession0, actualLocalClient, notifyChanges = true, reloadableOptions),
      ()
    )
    lazy val bloopSession0: BloopSession = BloopSession(inputs, remoteServer, bspServer, watcher)

    bloopSession0.registerWatchInputs()
    bspServer.newInputs(configDir, inputs)

    bloopSession0
  }

  private val bloopSession = new BloopSession.Reference

  /** The logic for the actual running of the `bsp` command, initializing the BSP connection.
    * @param initialInputs
    *   the initial input sources passed upon initializing the BSP connection (which are subject to
    *   change on subsequent workspace/reload requests)
    */
  def run(initialInputs: Seq[Module], initialBspOptions: BspReloadableOptions): Future[Unit] = {
    val logger    = initialBspOptions.logger
    val verbosity = initialBspOptions.verbosity

    actualLocalClient = new BspClient(
      threads.buildThreads.bloop.jsonrpc, // meh
      logger
    )
    localClient = getLocalClient(verbosity)

    val currentBloopSession = newBloopSession(initialInputs, initialBspOptions)
    bloopSession.update(null, currentBloopSession, "BSP server already initialized")

    val actualLocalServer
      : b.BuildServer with b.ScalaBuildServer with b.JavaBuildServer with b.JvmBuildServer
        with ScalaScriptBuildServer with HasGeneratedSources =
      new BuildServerProxy(
        () => bloopSession.get().bspServer,
        () => onReload()
      )

    val localServer: b.BuildServer with b.ScalaBuildServer with b.JavaBuildServer
      with b.JvmBuildServer with ScalaScriptBuildServer =
      if (verbosity >= 3)
        new LoggingBuildServerAll(actualLocalServer)
      else
        actualLocalServer

    val launcher = new jsonrpc.Launcher.Builder[b.BuildClient]()
      .setExecutorService(threads.buildThreads.bloop.jsonrpc) // FIXME No
      .setInput(in)
      .setOutput(out)
      .setRemoteInterface(classOf[b.BuildClient])
      .setLocalService(localServer)
      .create()
    val remoteClient = launcher.getRemoteProxy
    actualLocalClient.forwardToOpt = Some(remoteClient)

    localClient.onConnectWithServer(currentBloopSession.remoteServer.bloopServer.server)
    actualLocalClient.newInputs(configDir, initialInputs)
    currentBloopSession.resetDiagnostics(actualLocalClient)

    val recoverOnError: Scope => BuildException => Option[BuildException] = scope =>
      e => {
        actualLocalServer.targetScopeIdOpt(scope).foreach { targetScopeId =>
          actualLocalClient.reportBuildException(Some(targetScopeId), e)
        }

        logger.log(e)
        None
      }

    prepareBuild(
      currentBloopSession,
      initialBspOptions,
      maybeRecoverOnError = recoverOnError
    ) match {
      case Left((ex, scope)) => recoverOnError(scope)(ex)
      case Right(_)          =>
    }

    logger.log {
      val hasConsole = System.console() != null
      if (hasConsole)
        "Listening to incoming JSONRPC BSP requests, press Ctrl+D to exit."
      else
        "Listening to incoming JSONRPC BSP requests."
    }
    val f = launcher.startListening()

    val initiateFirstBuild: Runnable = { () =>
      try build(currentBloopSession, actualLocalClient, notifyChanges = false, initialBspOptions)
      catch {
        case t: Throwable =>
          logger.debug(s"Caught $t during initial BSP build, ignoring it")
      }
    }
    threads.prepareBuildExecutor.submit(initiateFirstBuild)

    val es = ExecutionContext.fromExecutorService(threads.buildThreads.bloop.jsonrpc)
    val futures = Seq(
      BspImpl.naiveJavaFutureToScalaFuture(f).map(_ => ())(es),
      currentBloopSession.bspServer.initiateShutdown
    )
    Future.firstCompletedOf(futures)(es)
  }

  /** Shuts down the current Bloop session
    */
  def shutdown(): Unit =
    for (currentBloopSession <- bloopSession.getAndNullify())
      currentBloopSession.dispose()

  /** BSP reload logic, to be used on a workspace/reload BSP request
    *
    * @param currentBloopSession
    *   the current Bloop session
    * @param previousInputs
    *   all the input sources present in the context before the reload
    * @param newInputs
    *   all the input sources to be included in the new context after the reload
    * @param reloadableOptions
    *   options which may be reloaded on a bsp workspace/reload request
    * @return
    *   a future containing a valid workspace/reload response
    */
  private def reloadBsp(
    currentBloopSession: BloopSession,
    previousInputs: Seq[Module],
    newInputs: Seq[Module],
    reloadableOptions: BspReloadableOptions
  ): CompletableFuture[AnyRef] = {
    val previousTargetIds = currentBloopSession.bspServer.targetIds
    val wasIntelliJ       = currentBloopSession.bspServer.isIntelliJ

    currentBloopSession.dispose()
    val newBloopSession0 = newBloopSession(newInputs, reloadableOptions, wasIntelliJ)
    bloopSession.update(currentBloopSession, newBloopSession0, "Concurrent reload of workspace")
    actualLocalClient.newInputs(configDir, newInputs)
    localClient.onConnectWithServer(newBloopSession0.remoteServer.bloopServer.server)

    newBloopSession0.resetDiagnostics(actualLocalClient)
    prepareBuild(newBloopSession0, reloadableOptions) match {
      case Left((buildException, scope)) =>
        CompletableFuture.completedFuture(
          responseError(
            s"Can't reload workspace, build failed for scope ${scope.name}: ${buildException.message}"
          )
        )
      case Right(preBuildProjects) =>
        val preBuildProject = preBuildProjects.head
        lazy val projectJavaHome = preBuildProject.mainScope.buildOptions
          .javaHome()
          .value

        val finalBloopSession =
          if (
            bloopSession.get().remoteServer.jvmVersion.exists(_.value < projectJavaHome.version)
          ) {
            reloadableOptions.logger.log(
              s"Bloop JVM version too low, current ${bloopSession.get().remoteServer.jvmVersion.get.value} expected ${projectJavaHome.version}, restarting server"
            )
            // RelodableOptions don't take into account buildOptions from sources
            val updatedReloadableOptions = reloadableOptions.copy(
              buildOptions =
                reloadableOptions.buildOptions orElse preBuildProject.mainScope.buildOptions,
              bloopRifleConfig = reloadableOptions.bloopRifleConfig.copy(
                javaPath = projectJavaHome.javaCommand,
                minimumBloopJvm = projectJavaHome.version
              )
            )

            newBloopSession0.dispose()
            val bloopSessionWithJvmOkay =
              newBloopSession(newInputs, updatedReloadableOptions, wasIntelliJ)
            bloopSession.update(
              newBloopSession0,
              bloopSessionWithJvmOkay,
              "Concurrent reload of workspace"
            )
            bloopSessionWithJvmOkay
          }
          else newBloopSession0

        val previousNames = previousInputs.map(_.projectName).toSet
        val currentNames  = preBuildProjects.map(_.mainScope.project.projectName).toSet
        val anyChanges    = previousNames != currentNames
        if (anyChanges)
          for (client <- finalBloopSession.bspServer.clientOpt) {
            val newTargetIds = finalBloopSession.bspServer.targetIds
            val events =
              newTargetIds.map(buildTargetIdToEvent(_, b.BuildTargetEventKind.CREATED)) ++
                previousTargetIds.map(buildTargetIdToEvent(_, b.BuildTargetEventKind.DELETED))
            val didChangeBuildTargetParams = new b.DidChangeBuildTarget(events.asJava)
            client.onBuildTargetDidChange(didChangeBuildTargetParams)
          }
    }
    CompletableFuture.completedFuture(new Object())
  }

  /** All the logic surrounding a workspace/reload (establishing the new inputs, settings and
    * refreshing all the relevant variables), including the actual BSP workspace reloading.
    *
    * @return
    *   a future containing a valid workspace/reload response
    */
  private def onReload(): CompletableFuture[AnyRef] = {
    val currentBloopSession = bloopSession.get()
    bspReloadableOptionsReference.reload()
    val reloadableOptions = bspReloadableOptionsReference.get
    val logger            = reloadableOptions.logger
    val verbosity         = reloadableOptions.verbosity
    actualLocalClient.logger = logger
    localClient = getLocalClient(verbosity)
    val workspace = configDir.getOrElse(currentBloopSession.modules.head.inputs.workspace)
    val ideInputsJsonPath =
      workspace / Constants.workspaceDirName / "ide-inputs.json"
    if (os.isFile(ideInputsJsonPath)) {
      val maybeResponse = either[BuildException] {
        val ideInputs = value {
          try Right(readFromArray(os.read.bytes(ideInputsJsonPath))(IdeInputs.codec))
          catch {
            case e: JsonReaderException =>
              logger.debug(s"Caught $e while decoding $ideInputsJsonPath")
              Left(new ParsingInputsException(e.getMessage, e))
          }
        }
        val newModules      = value(argsToInputs(ideInputs.args))
        val previousModules = currentBloopSession.modules
        val sameCount       = previousModules.sizeCompare(newModules) == 0
        def previousModulesSorted =
          if previousModules.sizeIs == 1 then previousModules
          else previousModules.sortBy(_.projectName)
        def newModulesSorted =
          if newModules.sizeIs == 1 then newModules else newModules.sortBy(_.projectName)

        val noChanges = sameCount && previousModulesSorted.lazyZip(newModulesSorted).forall {
          (previousModule, newModule) =>
            previousModule.projectName == newModule.projectName && {
              val newInputs      = newModule.inputs
              val newHash        = newModule.inputsHash
              val previousInputs = previousModule.inputs
              val previousHash   = previousModule.inputsHash
              newInputs == previousInputs && newHash == previousHash
            }
        }
        if noChanges then
          CompletableFuture.completedFuture(new Object)
        else
          reloadBsp(currentBloopSession, previousModules, newModules, reloadableOptions)
      }
      maybeResponse match {
        case Left(errorMessage) =>
          CompletableFuture.completedFuture(
            responseError(s"Workspace reload failed, couldn't load sources: $errorMessage")
          )
        case Right(r) => r
      }
    }
    else
      CompletableFuture.completedFuture(
        responseError(
          s"Workspace reload failed, inputs file missing from workspace directory: ${ideInputsJsonPath.toString()}"
        )
      )
  }
}

object BspImpl {

  private def buildTargetIdToEvent(
    targetId: b.BuildTargetIdentifier,
    eventKind: b.BuildTargetEventKind
  ): b.BuildTargetEvent = {
    val event = new b.BuildTargetEvent(targetId)
    event.setKind(eventKind)
    event
  }

  private def responseError(
    message: String,
    errorCode: Int = JsonRpcErrorCodes.InternalError
  ): ResponseError =
    new ResponseError(errorCode, message, new Object())

  // from https://github.com/com-lihaoyi/Ammonite/blob/7eb58c58ec8c252dc5bd1591b041fcae01cccf90/amm/interp/src/main/scala/ammonite/interp/script/AmmoniteBuildServer.scala#L550-L565
  private def naiveJavaFutureToScalaFuture[T](
    f: java.util.concurrent.Future[T]
  ): Future[T] = {
    val p = Promise[T]()
    val t = new Thread {
      setDaemon(true)
      setName("bsp-wait-for-exit")
      override def run(): Unit =
        p.complete {
          try Success(f.get())
          catch { case t: Throwable => Failure(t) }
        }
    }
    t.start()
    p.future
  }

  private final class LoggingBspClient(actualLocalClient: BspClient) extends LoggingBuildClient
      with BloopBuildClient {
    // in Scala 3 type of the method needs to be explicitly overridden
    def underlying: scala.build.bsp.BspClient = actualLocalClient
    def clear()                               = underlying.clear()
    def diagnostics                           = underlying.diagnostics
    def setProjectParams(newParams: Seq[String]) =
      underlying.setProjectParams(newParams)
    def setGeneratedSources(scope: Scope, newGeneratedSources: Seq[GeneratedSource]) =
      underlying.setGeneratedSources(scope, newGeneratedSources)
  }

  private final case class PreBuildData(
    sources: Sources,
    buildOptions: BuildOptions,
    classesDir: os.Path,
    scalaParams: Option[ScalaParameters],
    artifacts: Artifacts,
    project: Project,
    generatedSources: Seq[GeneratedSource],
    buildChanged: Boolean,
    dependsOn: List[String]
  )

  private final case class PreBuildProject(
    mainScope: PreBuildData,
    testScope: PreBuildData,
    module: Module,
    diagnostics: Seq[Diagnostic]
  )

  private object PreBuildProject {
    given asModule: ModuleLike[PreBuildProject] with {
      def id(t: PreBuildProject): String = t.module.projectName
      def weight(t: PreBuildProject): Int = 1 // if some targets take longer to build, we could factor that in here
      def dependsOn(t: PreBuildProject): Seq[String] =
        t.mainScope.dependsOn
    }
  }

  trait ModuleLike[T] {
    def id(t: T): String
    def weight(t: T): Int
    def dependsOn(t: T): Seq[String]
  }

  object ModuleLike {
    inline def apply[T](using m: ModuleLike[T]): m.type = m
  }

  object Dag {
    def topologicalSort[T: ModuleLike](modules: Seq[T]): Seq[Seq[T]] = {
      import scala.collection.mutable
      import scala.annotation.tailrec

      val byId = modules.map(m => ModuleLike[T].id(m) -> m).toMap
      val lookup = byId.map((id, m) => id -> ModuleLike[T].dependsOn(m))

      val outgoingEdges =
        lookup.map((k, v) => k -> v.map(dep => dep -> ModuleLike[T].weight(byId(dep))).to(mutable.LinkedHashMap))

      val incomingEdges: mutable.SeqMap[String, mutable.LinkedHashMap[String, Int]] =
        val buf = mutable.LinkedHashMap.empty[String, mutable.LinkedHashMap[String, Int]]
        for (k, v) <- lookup do
          buf.getOrElseUpdate(k, mutable.LinkedHashMap.empty[String, Int])
          for dep <- v do
            buf.getOrElseUpdate(dep, mutable.LinkedHashMap.empty[String, Int]).update(
              k,
              ModuleLike[T].weight(byId(dep))
            )
        buf

      val sNext     = mutable.LinkedHashSet.empty[String]
      val sLeftOver = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[String]]

      @tailrec
      def iterate(s1: List[String], s0: List[String], acc: List[List[String]]): List[List[T]] =
        sNext.clear()
        sLeftOver.clear()

        // reset weights of targets that are in s0
        for target <- s0 do
          val outgoing = outgoingEdges(target)
          for dep <- outgoing.keySet do
            val depM = byId(dep)
            val incoming = incomingEdges(dep)
            incoming(target) = ModuleLike[T].weight(depM)
            outgoing(dep) = ModuleLike[T].weight(depM)

        val sAll = s1 ::: s0

        val maxWeight   = sAll.flatMap(t => outgoingEdges(t).values.maxOption).maxOption.getOrElse(1)
        val minWeight   = sAll.flatMap(t => outgoingEdges(t).values.minOption).minOption.getOrElse(1)
        val decrementBy = minWeight

        extension [K](assoc: mutable.LinkedHashMap[K, Int])
          def decrement(t: K): Option[Int] =
            assoc.updateWith(t) {
              case Some(n) if n > decrementBy => Some(n - decrementBy)
              case _                          => None
            }

        def once(): Unit =
          for target <- sAll do
            val outgoing = outgoingEdges(target)
            for dep <- outgoing.keySet do
              val incoming = incomingEdges(dep)
              ((outgoing.decrement(dep), incoming.decrement(target)): @unchecked) match
                case (None, None) =>
                  sNext += dep
                case (Some(_), Some(_)) =>
                  sLeftOver.getOrElseUpdate(dep, mutable.LinkedHashSet.empty) += target

        var iterated  = 0
        var countDown = (maxWeight - minWeight) / decrementBy
        while
          once()
          iterated += 1
          sNext.isEmpty && countDown > 0
        do
          countDown -= 1
        if sNext.isEmpty then
          acc.map(_.map(byId(_)))
        else
          val s2    = sNext.toList
          val sRest = (sLeftOver --= s2).values.flatten.toList.distinct
          iterate(s2, sRest, s2 :: acc)
      end iterate

      val s0 = incomingEdges.collect { case (target, incoming) if incoming.isEmpty => target }.toList
      iterate(s0, Nil, s0 :: Nil)
    }
  }
}
