package scala.compose.commands.run

import ai.kien.python.Python
import caseapp.*
import caseapp.core.help.HelpFormat

import java.io.File
import java.util.Locale
import java.util.concurrent.CompletableFuture

import scala.build.EitherCps.{either, value}
import scala.build.*
import scala.build.errors.BuildException
import scala.build.input.{Inputs, ScalaCliInvokeData, SubCommand}
import scala.build.internal.util.ConsoleUtils.ScalaCliConsole
import scala.build.internal.{Constants, Runner, ScalaJsLinkerConfig}
import scala.build.options.{BuildOptions, JavaOpt, Platform, ScalacOpt}
import scala.cli.CurrentParams
import scala.cli.commands.package0.Package
import scala.cli.commands.publish.ConfigUtil.*
import scala.cli.commands.run.Run.SpecificationLevel
import scala.cli.commands.run.{RunMode, RunOptions}
import scala.cli.commands.setupide.SetupIde
import scala.cli.commands.shared.{HelpCommandGroup, HelpGroup, SharedOptions}
import scala.cli.commands.update.Update
import scala.cli.commands.util.{BuildCommandHelpers, RunHadoop, RunSpark}
import scala.cli.commands.{CommandUtils, ScalaCommand, SpecificationLevel, WatchUtil}
import scala.cli.config.{ConfigDb, Keys}
import scala.cli.internal.ProcUtil
import scala.cli.util.ArgHelpers.*
import scala.cli.util.ConfigDbUtils
import scala.util.{Properties, Try}

object Run extends ScalaCommand[RunOptions] with BuildCommandHelpers {
  private[compose] var rawArgs = Array.empty[String]

  override def sharedOptions(options: RunOptions): Option[SharedOptions] = Some(options.shared)
  override def scalaSpecificationLevel: SpecificationLevel               = SpecificationLevel.MUST
  override def runCommand(options: RunOptions, args: RemainingArgs, logger: Logger): Unit = {
    println("Hello Scala Compose")
  }

}
