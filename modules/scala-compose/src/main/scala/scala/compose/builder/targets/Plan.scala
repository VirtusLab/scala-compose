package scala.compose.builder.targets

import scala.compose.builder.Settings
import scala.compose.builder.ModuleGraph
import scala.compose.builder.Module
import scala.compose.builder.ModuleKind
import scala.compose.builder.settings
import scala.compose.builder.errors.Result
import scala.compose.builder.errors.failure
import scala.compose.builder.errors.CanError
import scala.compose.builder.reporter
import scala.compose.builder.ScalaCommand
import scala.compose.builder.ScalaCommand.SubCommand

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.compose.builder.PlatformKind

/** A Plan operates over the target graph and produces a new one */
sealed trait Plan:
  /** executes the step, and signals if the project changed */
  def exec(initial: Targets)(using Settings): Result[Targets, String]

object Plan:

  def compile(targetModules: Set[Module], subcommand: SubCommand)(using
    Settings
  ): Result[Plan, String] =
    Result {
      val graph  = TargetGraph.compile(settings.config.modules, targetModules.toSeq, subcommand).?
      val stages = graph.stages

      reporter.debug(
        s"compilation plan for command $subcommand ${targetModules.map(_.name).mkString(", ")}:\n${graph.show}"
      )

      def lookup(name: String) = settings.config.modules(name)

      val stepss = stages.map { stage =>
        val steps: List[Step] = stage.map { target =>
          target.kind match
            case TargetKind.Library(platform) =>
              CompileScalaStep(lookup(target.module), target, platform)
            case TargetKind.Application =>
              val module = lookup(target.module)
              RunScalaStep(module, module.kind.asApplication, target, module.platforms.head)
            case TargetKind.Package =>
              val module = lookup(target.module)
              PackageScalaStep(module, module.kind.asApplication, target, module.platforms.head)
            case TargetKind.Copy(fromTarget) =>
              CopyResourceStep(lookup(target.module), target, fromTarget)
        }
        steps
      }

      if settings.sequential then
        new Plan:
          def exec(initial: Targets)(using Settings): Result[Targets, String] =
            Result {
              stepss.foldLeft(initial) { (curr, steps) =>
                val updates = for step <- steps yield Shared.runStep(step, curr, initial).?
                curr ++ updates.flatten
              }
            }
        end new
      else
        new Plan:
          def exec(initial: Targets)(using Settings): Result[Targets, String] =
            Result {
              stepss.foldLeft(initial) { (curr, steps) =>
                val updateResultsF = Future.sequence {
                  for step <- steps yield blocking {
                    Future {
                      Shared.runStep(step, curr, initial)
                    }
                  }
                }
                val updateResults = Await.result(updateResultsF, Duration.Inf)
                curr ++ updateResults.flatMap(_.?)
              }
            }
        end new
      end if
    }
  end compile
end Plan
