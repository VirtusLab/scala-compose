package scala.compose

import caseapp.core.app.CommandsEntryPoint
import caseapp.core.help.{Help, HelpFormat, RuntimeCommandsHelp}

import java.nio.file.InvalidPathException

import scala.cli.commands.shared.ScalaCliHelp
import scala.cli.commands.{NeedsArgvCommand, ScalaCommand}
import scala.compose.commands.*

class ScalaComposeCommands(
  val progName: String
) extends CommandsEntryPoint {

  lazy val actualDefaultCommand = run.Run

  private def allCommands = Seq[ScalaCommand[_]](
    compile.Compile,
    run.Run,
    test.Test
  )

  def commands =
    allCommands

  final override def defaultCommand = Some(actualDefaultCommand)

  // FIXME Report this in case-app default NameFormatter
  override lazy val help: RuntimeCommandsHelp = {
    val parent = super.help
    parent.copy(defaultHelp = Help[Unit]())
  }

  override def enableCompleteCommand    = true
  override def enableCompletionsCommand = true

  override def helpFormat: HelpFormat = ScalaCliHelp.helpFormat

  override def main(args: Array[String]): Unit = {

    // quick hack, until the raw args are kept in caseapp.RemainingArgs by case-app
    actualDefaultCommand.rawArgs = args

    commands.foreach {
      case c: NeedsArgvCommand => c.setArgv(progName +: args)
      case _                   =>
    }
    actualDefaultCommand.setArgv(progName +: args)

    super.main(args)
  }
}
