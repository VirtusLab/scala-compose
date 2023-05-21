package scala.compose.commands.run

import caseapp.*
import caseapp.core.help.HelpFormat

import java.io.File
import scala.build.EitherCps.{either, value}
import scala.build.errors.BuildException
import scala.build.options.{BuildOptions, Platform, Scope}
import scala.build.{Build, BuildThreads, Builds, Logger, Os}
import scala.build.internal.util.ConsoleUtils.ScalaCliConsole
import scala.cli.CurrentParams
import scala.cli.commands.publish.ConfigUtil.*
import scala.cli.commands.run.Run.{SpecificationLevel, maybeRunOnce}
import scala.cli.commands.shared.{HelpCommandGroup, HelpGroup, SharedOptions}
import scala.cli.commands.update.Update
import scala.cli.commands.util.BuildCommandHelpers
import scala.cli.commands.{CommandUtils, ScalaCommand, SpecificationLevel, WatchUtil}
import scala.cli.config.{ConfigDb, Keys}
import scala.cli.util.ArgHelpers.*
import scala.cli.util.ConfigDbUtils
import scala.build.options.PackageType
import scala.compose.commands.setupide.SetupIde
import scala.compose.BuildModules.{ScalaComposeException, *}
import scala.compose.builder.Module
import scala.compose.builder.errors.Result
import scala.compose.builder.{ModuleKind, configFile, parseConfig}
import scala.cli.commands.run.{RunMode, RunOptions}
import scala.cli.commands.run.Run.maybeRunOnce0
import scala.cli.internal.ProcUtil
import java.util.concurrent.CompletableFuture
import scala.build.internal.Constants

object Run extends ScalaCommand[RunOptions] with BuildCommandHelpers {
  private[compose] var rawArgs = Array.empty[String]

  override def sharedOptions(options: RunOptions): Option[SharedOptions] = Some(options.shared)
  override def scalaSpecificationLevel: SpecificationLevel               = SpecificationLevel.MUST

  private def runMode(options: RunOptions): RunMode =
    if (
      options.sharedRun.standaloneSpark.getOrElse(false) &&
      !options.sharedRun.sparkSubmit.contains(false)
    )
      RunMode.StandaloneSparkSubmit(options.sharedRun.submitArgument)
    else if (options.sharedRun.sparkSubmit.getOrElse(false))
      RunMode.SparkSubmit(options.sharedRun.submitArgument)
    else if (options.sharedRun.hadoopJar)
      RunMode.HadoopJar
    else
      RunMode.Default

  override def runCommand(options: RunOptions, args: RemainingArgs, logger: Logger): Unit =
    runCommand(
      options,
      args.remaining,
      args.unparsed,
      logger
    )

  def runCommand(
    options: RunOptions,
    inputArgs: Seq[String],
    programArgs: Seq[String],
    logger: Logger
  ): Unit = {
    val buildOptions = buildOptionsOrExit(options)
    val configDir    = Os.pwd

    val configLocation = configFile(Some(configDir)).asBuildFailure.orExit(logger)
    val config         = parseConfig(configLocation).asBuildFailure.orExit(logger)

    val toRun =
      inputArgs.headOption.toRight(ScalaComposeException("No module supplied")).orExit(logger)
    config.modules.get(toRun) match
      case Some(module) if module.kind.isApplication =>
        ()
      case Some(module) =>
        System.err.println(s"module $toRun has unexpected kind ${module.kind}")
        sys.exit(1)
      case _ =>
        System.err.println(s"There does not exist an application module $toRun")
        sys.exit(1)

    val modules = readModules(options.shared.inputs(_), config, configDir).orExit(logger)

    CurrentParams.workspaceOpt = Some(configDir)

    SetupIde.runSafe(
      options.shared,
      configDir,
      inputArgs
    )

    val threads       = BuildThreads.create()
    val compilerMaker = options.shared.compilerMaker(threads).orExit(logger)
    val configDb      = ConfigDbUtils.configDb.orExit(logger)
    val actionableDiagnostics =
      options.shared.logging.verbosityOptions.actions.orElse(
        configDb.get(Keys.actions).getOrElse(None)
      )

    def postBuild(module: String, builds: Builds, allowExit: Boolean): Unit = {
      val failed = builds.all.exists {
        case _: Build.Failed => true
        case _               => false
      }
      val cancelled = builds.all.exists {
        case _: Build.Cancelled => true
        case _                  => false
      }
      if (failed) {
        System.err.println(s"Compilation failed for $module")
        if (allowExit)
          sys.exit(1)
      }
      else if (cancelled) {
        System.err.println(s"Compilation cancelled for $module")
        if (allowExit)
          sys.exit(1)
      }
      else {
        val successulBuildOpt =
          for {
            build <- builds.get(Scope.Test).orElse(builds.get(Scope.Main))
            s     <- build.successfulOpt
          } yield s
        //        if (options.printClassPath)
        //          for (s <- successulBuildOpt) {
        //            val cp = s.fullClassPath.map(_.toString).mkString(File.pathSeparator)
        //            println(cp)
        //          }
        successulBuildOpt.foreach(_.copyOutput(options.shared))
      }
    }

    val buildsMap = Build.build0(
      modules,
      buildOptions,
      compilerMaker,
      None,
      logger,
      crossBuilds = false,
      buildTests = false,
      partial = None,
      actionableDiagnostics = actionableDiagnostics,
      configDir = Some(configDir)
    ).orExit(logger)

    def copyDepOuts(module: Module): Unit =
      module.dependsOn.foreach { dep =>
        buildsMap(dep).foreach(postBuild(module.name, _, allowExit = true))
        copyDepOuts(config.modules(dep))
      }

    def maybeRun(
      build: Build.Successful,
      allowTerminate: Boolean,
      runMode: RunMode,
      showCommand: Boolean,
      scratchDirOpt: Option[os.Path]
    ): Either[BuildException, Option[(Process, CompletableFuture[_])]] = either {
      val potentialMainClasses = build.foundMainClasses()
      if (options.sharedRun.mainClass.mainClassLs.contains(true))
        value {
          options.sharedRun.mainClass
            .maybePrintMainClasses(potentialMainClasses, shouldExit = allowTerminate)
            .map(_ => None)
        }
      else {
        val processOrCommand = value {
          maybeRunOnce0(
            build,
            programArgs,
            logger,
            allowExecve = allowTerminate,
            jvmRunner = build.artifacts.hasJvmRunner,
            potentialMainClasses,
            runMode,
            showCommand,
            scratchDirOpt
          )
        }

        processOrCommand match {
          case Right((process, onExitOpt)) =>
            val onExitProcess = process.onExit().thenApply { p1 =>
              val retCode = p1.exitValue()
              onExitOpt.foreach(_())
              (retCode, allowTerminate) match {
                case (0, true) =>
                case (0, false) =>
                  val gray  = ScalaCliConsole.GRAY
                  val reset = Console.RESET
                  System.err.println(s"${gray}Program exited with return code $retCode.$reset")
                case (_, true) =>
                  sys.exit(retCode)
                case (_, false) =>
                  val red      = Console.RED
                  val lightRed = "\u001b[91m"
                  val reset    = Console.RESET
                  System.err.println(
                    s"${red}Program exited with return code $lightRed$retCode$red.$reset"
                  )
              }
            }

            Some((process, onExitProcess))

          case Left(command) =>
            for (arg <- command)
              println(arg)
            None
        }
      }
    }

    buildsMap.get(toRun) match {
      case Some(buildsSeq) => buildsSeq match
          case Seq(builds) =>
            builds.main match
              case s: Build.Successful =>
                copyDepOuts(config.modules(toRun))
                s.copyOutput(options.shared)
                val res = maybeRun(
                  s,
                  allowTerminate = true,
                  runMode = runMode(options),
                  showCommand = options.sharedRun.command,
                  scratchDirOpt = None // TODO: fill in
                )
                  .orExit(logger)
                for ((process, onExit) <- res)
                  ProcUtil.waitForProcess(process, onExit)
              case _: Build.Failed =>
                System.err.println("Compilation failed")
                sys.exit(1)
          case _ =>
            System.err.println(s"module $toRun does not have a single build.")
            sys.exit(1)
      case None =>
        System.err.println(s"could not find build for $toRun.")
        sys.exit(1)
    }

  }

}
