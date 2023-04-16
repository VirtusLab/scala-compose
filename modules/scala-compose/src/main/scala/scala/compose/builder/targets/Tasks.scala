package scala.compose.builder.targets

import scala.compose.builder.reporter
import scala.compose.builder.errors.*

import scala.compose.builder.{Module, ModuleKind}
import scala.compose.builder.{settings, Settings}

import scala.compose.builder.ScalaCommand, ScalaCommand.SubCommand

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.compose.builder.PlatformKind

/** Tasks operate on the target graph, i.e. they do not produce cacheable results. */
object Tasks:

  def clean(modules: Set[Module])(using Settings): Result[Unit, String] =

    def cleanOne(module: Module): Result[Unit, String] =
      Result {
        reporter.info(s"cleaning module ${module.name}...")
        Shared.doCleanModule(module).?
        Shared.clearAndRemoveDir(os.pwd / ".scala-builder" / module.name).?
      }

    val results =
      if settings.sequential then
        for module <- modules.toList yield
          cleanOne(module)
      else
        val futures = for module <- modules.toList yield
          Future {
            blocking {
              cleanOne(module)
            }
          }
        Await.result(Future.sequence(futures), Duration.Inf)
    Result {
      results.foreach(_.?)
    }
  end clean

  def run(module: Module, info: ModuleKind.Application, project: Targets)(using Settings): Result[Unit, String] =
    Result {
      val target = project.application(module.name)
      val appCommand = target.outCommand

      val mainMessage = info.mainClass match
        case None => failure("no main class specified, TODO: interactive mode")
        case Some(value) => s"with specified main class $value"

      reporter.debug(s"running command: ${appCommand.mkString(" ")}")
      val result = ScalaCommand.spawn(List(appCommand)).?

      if !result.join() then
        failure(s"failure with exit code ${result.exitCode()}")
    }

  def repl(module: Module, project: Targets)(using Settings): Result[Unit, String] =
    def resourceDir = os.pwd / ".scala-builder" / module.name / "managed_resources"

    Result {
      val target = project.library(module.name, PlatformKind.jvm)
      val classpath = target.extraClasspath
      val dependencies = target.extraDependencies
      val resourceArgs = Shared.resourceArgs(module)
      val args = ScalaCommand.makeArgs(module, SubCommand.Repl, classpath, dependencies, PlatformKind.jvm, resourceArgs)
      reporter.debug(s"running command: ${args.map(_.value.mkString(" ")).mkString(" ")}")
      val result = ScalaCommand.spawn(args).?

      if !result.join() then
        failure(s"failure with exit code ${result.exitCode()}")
    }

  def test(modules: Set[Module], project: Targets, initial: Targets)(using Settings): Result[Unit, String] =
    def testOne(module: Module): Result[Unit, String] =
      def resourceDir = os.pwd / ".scala-builder" / module.name / "managed_resources"
      Result {
        val deps = Shared.dependencies(module, PlatformKind.jvm, project, initial)

        if deps.changedState then
          Shared.cleanBeforeCompile(module).?

        val resourceArgs = Shared.resourceArgs(module)
        val args = ScalaCommand.makeArgs(module, SubCommand.Test, deps.classpath, deps.libraries, PlatformKind.jvm, resourceArgs)
        reporter.debug(s"running command: ${args.map(_.value.mkString(" ")).mkString(" ")}")
        val result = ScalaCommand.spawn(args).?

        if !result.join() then
          failure(s"failure with exit code ${result.exitCode()}")
      }
    Result {
      if settings.sequential then
        for module <- modules do
          testOne(module).?
      else
        val futures = for module <- modules.toList yield
          Future {
            blocking {
              testOne(module)
            }
          }
        val results = Await.result(Future.sequence(futures), Duration.Inf)
        results.foreach(_.?)
    }
