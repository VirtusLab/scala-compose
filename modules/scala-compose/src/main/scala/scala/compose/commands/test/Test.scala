package scala.compose.commands.test

import caseapp.*
import caseapp.core.help.HelpFormat

import java.nio.file.Path

import scala.build.EitherCps.{either, value}
import scala.build.Ops.*
import scala.build.*
import scala.build.errors.{BuildException, CompositeBuildException}
import scala.build.internal.util.ConsoleUtils.ScalaCliConsole
import scala.build.internal.{Constants, Runner}
import scala.build.options.{BuildOptions, JavaOpt, Platform, Scope}
import scala.build.testrunner.AsmTestRunner
import scala.cli.CurrentParams
import scala.cli.commands.publish.ConfigUtil.*
import scala.cli.commands.run.Run
import scala.cli.commands.run.Run.SpecificationLevel
import scala.cli.commands.setupide.SetupIde
import scala.cli.commands.shared.{HelpCommandGroup, HelpGroup, SharedOptions}
import scala.cli.commands.test.TestOptions
import scala.cli.commands.update.Update
import scala.cli.commands.{CommandUtils, ScalaCommand, SpecificationLevel, WatchUtil}
import scala.cli.config.{ConfigDb, Keys}
import scala.cli.util.ArgHelpers.*
import scala.cli.util.ConfigDbUtils

object Test extends ScalaCommand[TestOptions] {
  override def sharedOptions(options: TestOptions): Option[SharedOptions] = Some(options.shared)
  override def scalaSpecificationLevel: SpecificationLevel                = SpecificationLevel.MUST

  override def runCommand(options: TestOptions, args: RemainingArgs, logger: Logger): Unit = {
    ???
  }

}
