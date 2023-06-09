package scala.build.bsp

import java.io.{InputStream, OutputStream}

import scala.build.errors.BuildException
import scala.build.input.Inputs
import scala.concurrent.Future

trait Bsp {
  def run(initialInputs: Seq[Module]): Future[Unit]
  def shutdown(): Unit
}

object Bsp {
  def create(
    argsToInputs: Seq[String] => Either[BuildException, Seq[Module]],
    bspReloadableOptionsReference: BspReloadableOptions.Reference,
    threads: BspThreads,
    in: InputStream,
    out: OutputStream,
    actionableDiagnostics: Option[Boolean],
    configDir: Option[os.Path]
  ): Bsp =
    new BspImpl(
      argsToInputs,
      bspReloadableOptionsReference,
      threads,
      in,
      out,
      actionableDiagnostics,
      configDir
    )
}
