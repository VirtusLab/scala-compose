package scala.compose.commands.compile

import caseapp.*
import caseapp.core.help.HelpFormat

import java.io.File

import scala.build.options.{BuildOptions, Scope}
import scala.build.{Build, BuildThreads, Builds, Logger, Os}
import scala.cli.CurrentParams
import scala.cli.commands.compile.CompileOptions
import scala.cli.commands.publish.ConfigUtil.*
import scala.cli.commands.run.Run.SpecificationLevel
import scala.cli.commands.setupide.SetupIde
import scala.cli.commands.shared.{HelpCommandGroup, HelpGroup, SharedOptions}
import scala.cli.commands.update.Update
import scala.cli.commands.util.BuildCommandHelpers
import scala.cli.commands.{CommandUtils, ScalaCommand, SpecificationLevel, WatchUtil}
import scala.cli.config.{ConfigDb, Keys}
import scala.cli.util.ArgHelpers.*
import scala.cli.util.ConfigDbUtils

object Compile extends ScalaCommand[CompileOptions] with BuildCommandHelpers {

  override def sharedOptions(options: CompileOptions): Option[SharedOptions] = Some(options.shared)
  override def scalaSpecificationLevel: SpecificationLevel = SpecificationLevel.MUST

  override def runCommand(options: CompileOptions, args: RemainingArgs, logger: Logger): Unit = {
    ???
  }

}
