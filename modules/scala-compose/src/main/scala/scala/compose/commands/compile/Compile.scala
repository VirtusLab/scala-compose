package scala.compose.commands.compile

import caseapp.*
import caseapp.core.help.HelpFormat

import java.io.File
import scala.build.EitherCps.{either, value}
import scala.build.errors.BuildException
import scala.build.options.{BuildOptions, Platform, Scope}
import scala.build.{Build, BuildThreads, Builds, Logger, Os}
import scala.cli.CurrentParams
import scala.cli.commands.publish.ConfigUtil.*
import scala.cli.commands.run.Run.SpecificationLevel
import scala.cli.commands.shared.{HelpCommandGroup, HelpGroup, SharedOptions}
import scala.cli.commands.update.Update
import scala.cli.commands.util.BuildCommandHelpers
import scala.cli.commands.{CommandUtils, ScalaCommand, SpecificationLevel, WatchUtil}
import scala.cli.config.{ConfigDb, Keys}
import scala.cli.util.ArgHelpers.*
import scala.cli.util.ConfigDbUtils
import scala.build.options.PackageType
import scala.compose.commands.setupide.SetupIde
import scala.compose.BuildModules.*
import scala.compose.builder.errors.Result
import scala.compose.builder.{configFile, parseConfig}

object Compile extends ScalaCommand[CompileOptions] with BuildCommandHelpers {

  override def sharedOptions(options: CompileOptions): Option[SharedOptions] = Some(options.shared)
  override def scalaSpecificationLevel: SpecificationLevel = SpecificationLevel.MUST

  override def runCommand(options: CompileOptions, args: RemainingArgs, logger: Logger): Unit = {
    val buildOptions = buildOptionsOrExit(options)
    val configDir    = Os.pwd

    val configLocation = configFile(Some(configDir)).asBuildFailure.orExit(logger)
    val config         = parseConfig(configLocation).asBuildFailure.orExit(logger)

    val modules = readModules(options.shared.inputs(_), config, configDir).orExit(logger)

    CurrentParams.workspaceOpt = Some(configDir)

    SetupIde.runSafe(
      options.shared,
      configDir,
      args.all
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
    for (module, buildsSeq) <- buildsMap do
      buildsSeq.foreach(postBuild(module, _, allowExit = true))
  }

}
