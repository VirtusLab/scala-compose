package scala.compose.builder

import toml.Value.*
import upickle.default.*
import scala.compose.builder.errors.*

object Config:

  def parse(text: String): Result[Config, String] =
    toml.Toml.parse(text) match
      case Left((_, msg)) => Result.Failure(msg)
      case Right(value) => readConfig(value)

  private def readConfig(table: Tbl): Result[Config, String] =
    Result {
      Config(
        scalaVersion = table.values.get("scalaVersion").map({
          case Str(value) => value
          case _ => failure("scalaVersion must be a string")
        }),
        modules = table.values.get("modules").map({
          case Tbl(values) =>
            val parsed = values.view.map(readModule.tupled.?)
            ModuleGraph.assemble(parsed.toList).?
          case _ => failure("modules must be a table")
        }).getOrElse(Map.empty)
      )
    }

  private def readModule[T](key: String, value: toml.Value): Result[Module, String] = Result {
    value match
      case Tbl(values) =>
        val root = values.get("root").map({
          case Str(value) => value
          case _ => failure(s"modules.${key}.root must be a string")
        }).getOrElse(key)
        val kind = values.get("kind").map({
          case Str(value) => value
          case _ => failure(s"modules.${key}.kind must be a string")
        }).getOrElse("library")
        val platforms = values.get("platforms").map({
          case Arr(values) => values.map({
            case Str(value) => PlatformKind.lookup(value) match
              case None => failure(s"unknown platform for modules.${key}.platforms: $value")
              case Some(platform) => platform

            case _ => failure(s"modules.${key}.platforms must be a list of strings")
          })
          case _ => failure(s"modules.${key}.platforms must be a list of strings")
        }).getOrElse(List(PlatformKind.jvm))
        val dependsOn = values.get("dependsOn").map({
          case Arr(values) => values.map({
            case Str(value) => value
            case _ => failure(s"modules.${key}.dependsOn must be a list of strings")
          })
          case _ => failure(s"modules.${key}.dependsOn must be a list of strings")
        }).getOrElse(Nil)
        val mainClass = values.get("mainClass").map({
          case Str(value) => value
          case _ => failure(s"modules.${key}.mainClass must be a string")
        })
        val moduleKind = kind match
          case "library" => ModuleKind.Library
          case "application" => ModuleKind.Application(mainClass = mainClass)
          case _ => failure(s"unknown module kind for modules.${key}.kind: $kind")
        if !moduleKind.isInstanceOf[ModuleKind.Application] && mainClass.nonEmpty then
          failure(s"modules.${key}.mainClass is only valid for application modules")
        // resourceGenerators = [{ module = "webpage", dest = "assets/main.js" }]
        val resourceGenerators = values.get("resourceGenerators").map({
          case Arr(values) => values.map(readResourceGenerator(key, _).?)
          case _ => failure(s"modules.${key}.resourceGenerators must be a list of tables")
        }).getOrElse(Nil)
        Module(
          name = key,
          root = root,
          platforms = platforms,
          kind = moduleKind,
          dependsOn = dependsOn,
          resourceGenerators = resourceGenerators
        )
      case _ =>
        failure(s"module.$key must be a table")
  }

  private def readResourceGenerator[T](module: String, value: toml.Value): Result[ResourceGenerator, String] =
    Result {
      value match
        case Tbl(values) =>
          val fromModule = values.get("module").map({
            case Str(value) => value
            case _ => failure(s"modules.${module}.resourceGenerators.module must be a string")
          }).getOrElse(failure(s"modules.${module}.resourceGenerators.module must be a string"))
          val dest = values.get("dest").map({
            case Str(value) => value
            case _ => failure(s"modules.${module}.resourceGenerators.dest must be a string")
          }).getOrElse(failure(s"modules.${module}.resourceGenerators.dest must be a string"))
          val target = targets.Target(fromModule, targets.TargetKind.Package)
          ResourceGenerator.Copy(target = target, dest = dest)
        case _ => failure(s"modules.${module}.resourceGenerators must be a list of tables")
    }
  end readResourceGenerator

case class Config(
  scalaVersion: Option[String] = None,
  modules: Map[String, Module] = Map.empty
) derives ReadWriter

case class Module(
  name: String,
  root: String,
  platforms: List[PlatformKind] = List(PlatformKind.`jvm`),
  kind: ModuleKind = ModuleKind.Library,
  dependsOn: List[String] = Nil,
  resourceGenerators: List[ResourceGenerator] = Nil
) derives ReadWriter

enum PlatformKind derives ReadWriter:
  case `jvm`, `scala-js`, `scala-native`

object PlatformKind:
  private val byName = values.view.map(kind => kind.toString -> kind).toMap

  def lookup(str: String): Option[PlatformKind] = byName.get(str)

enum ModuleKind derives ReadWriter:
  case Library
  case Application(mainClass: Option[String])

  // inline def asApplication(using inline ce: CanError[String]): Application = this match
  inline def asApplication(using inline ce: CanError[String]): Application = this match
    case app: Application => app
    case _ => failure("expected application module")

enum ResourceGenerator derives ReadWriter:
  case Copy(target: targets.Target, dest: String)
