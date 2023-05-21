package scala.compose.commands.setupide

import caseapp.*
import ch.epfl.scala.bsp4j.BspConnectionDetails
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.google.gson.GsonBuilder

import java.nio.charset.Charset

import scala.build.EitherCps.{either, value}
import scala.build.*
import scala.build.bsp.IdeInputs
import scala.build.errors.{BuildException, WorkspaceError}
import scala.build.input.{Inputs, OnDisk, Virtual, WorkspaceOrigin}
import scala.build.internal.{Constants, CustomCodeWrapper}
import scala.build.options.{BuildOptions, Scope}
import scala.cli.CurrentParams
import scala.cli.commands.shared.{SharedBspFileOptions, SharedOptions}
import scala.cli.commands.{CommandUtils, ScalaCommand}
import scala.cli.errors.FoundVirtualInputsError
import scala.jdk.CollectionConverters.*

object SetupIde extends ScalaCommand[SetupIdeOptions] {

  override def scalaSpecificationLevel = SpecificationLevel.IMPLEMENTATION

  override def runCommand(options: SetupIdeOptions, args: RemainingArgs, logger: Logger): Unit = {
    CurrentParams.workspaceOpt = Some(options.confDirPath)

    val bspPath = writeBspConfiguration(
      options,
      args = args.all
    ).orExit(logger)

    bspPath.foreach(path => println(s"Wrote configuration file for ide in: $path"))
  }

  override def sharedOptions(options: SetupIdeOptions): Option[SharedOptions] = Some(options.shared)

  def runSafe(
    options: SharedOptions,
    configDir: os.Path,
    args: Seq[String]
  ): Unit =
    writeBspConfiguration(
      SetupIdeOptions(shared = options, confDir = configDir.toNIO.toString),
      args
    ) match {
      case Left(ex) =>
        logger.debug(s"Ignoring error during setup-ide: ${ex.message}")
      case Right(_) =>
    }

  private def writeBspConfiguration(
    options: SetupIdeOptions,
    args: Seq[String]
  ): Either[BuildException, Option[os.Path]] = either {

    val progName = argvOpt.flatMap(_.headOption).getOrElse {
      sys.error("setup-ide called in a non-standard way :|")
    }

    val logger = options.shared.logger

    val (bspName, bspJsonDestination) = bspDetails(options.confDirPath, options.bspFile)
    val scalaCliBspJsonDestination =
      options.confDirPath / Constants.workspaceDirName / "ide-options-v2.json"
    val scalaCliBspInputsJsonDestination =
      options.confDirPath / Constants.workspaceDirName / "ide-inputs.json"

    val ideInputs = IdeInputs(
      options.shared.validateInputArgs(args)
        .flatMap(_.toOption)
        .flatten
        .collect { case d: OnDisk => d.path.toString }
    )

    val debugOpt = options.shared.jvm.bspDebugPort.toSeq.map(port =>
      s"-J-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:$port,suspend=y"
    )

    val bspArgs =
      List(CommandUtils.getAbsolutePathToScalaCli(progName), "bsp") ++
        debugOpt ++
        List("--conf-dir", options.confDirPath.toString)
    val details = new BspConnectionDetails(
      bspName,
      bspArgs.asJava,
      Constants.version,
      bloop.rifle.internal.Constants.bspVersion,
      List("scala", "java").asJava
    )

    val charset = options.charset
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Charset.forName)
      .getOrElse(Charset.defaultCharset()) // Should it be UTF-8?

    val gson = new GsonBuilder().setPrettyPrinting().create()

    val json                      = gson.toJson(details)
    val scalaCliOptionsForBspJson = writeToArray(options.shared)(SharedOptions.jsonCodec)
    val scalaCliBspInputsJson     = writeToArray(ideInputs)

    os.write.over(bspJsonDestination, json.getBytes(charset), createFolders = true)
    os.write.over(
      scalaCliBspJsonDestination,
      scalaCliOptionsForBspJson,
      createFolders = true
    )
    os.write.over(
      scalaCliBspInputsJsonDestination,
      scalaCliBspInputsJson,
      createFolders = true
    )
    logger.debug(s"Wrote $bspJsonDestination")
    Some(bspJsonDestination)
  }

  def bspDetails(workspace: os.Path, ops: SharedBspFileOptions): (String, os.Path) = {
    import ops.*
    val dir = bspDirectory
      .filter(_.nonEmpty)
      .map(os.Path(_, Os.pwd))
      .getOrElse(workspace / ".bsp")
    val bspName0 = bspName.map(_.trim).filter(_.nonEmpty).getOrElse(baseRunnerName)

    (bspName0, dir / s"$bspName0.json")
  }
}
