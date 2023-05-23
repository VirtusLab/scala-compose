package scala.compose.builder

import ConsoleSubCommand.*
import upickle.default.*

import scala.compose.builder.targets.*
import scala.compose.builder.errors.*
import scala.compose.builder.ScalaCommand.SubCommand

import java.io.IOException

@main def runner(args: String*): Unit =
  Result {
    execCommand(ConsoleCommand.parse(args.toList).?).?
  }.match
    case Result.Failure(err) => reporter.error(err)
    case Result.Success(())  => ()

enum ConsoleSubCommand:
  case Clean, ShowConfig
  case Run(project: Option[String])
  case Repl(project: String)
  case Test(projects: List[String])
  case Validate

case class ConsoleCommand(sub: ConsoleSubCommand, debug: Boolean, sequential: Boolean)

def settings(using Settings): Settings = summon[Settings]

case class Settings(debug: Boolean, sequential: Boolean, config: Config)

private lazy val cachePath = os.pwd / ".scala-builder" / "cache.json"

object ConsoleCommand:

  private def bool(args: List[String], flag: String): Boolean =
    args.contains(flag)

  def parse(args: List[String]): Result[ConsoleCommand, String] =
    Result {
      args match
        case "run" :: args =>
          val (project, args1) = args match
            case arg :: rest if !arg.startsWith("-") =>
              (Some(arg), rest)
            case _ =>
              (None, args)
          ConsoleCommand(
            Run(project),
            debug = bool(args1, "--debug"),
            sequential = bool(args1, "--sequential")
          )
        case "clean" :: args => ConsoleCommand(
            Clean,
            debug = args.contains("--debug"),
            sequential = bool(args, "--sequential")
          )
        case "test" :: args =>
          val (projects, args1) = args match
            case arg :: rest if !arg.startsWith("-") =>
              (arg.split(":").toList, rest)
            case _ =>
              (Nil, args)
          ConsoleCommand(
            Test(projects),
            debug = args1.contains("--debug"),
            sequential = bool(args, "--sequential")
          )
        case "repl" :: Nil => failure("missing project name for `repl` command")
        case "repl" :: project :: args => ConsoleCommand(
            Repl(project),
            debug = args.contains("--debug"),
            sequential = bool(args, "--sequential")
          )
        case "show-config" :: args => ConsoleCommand(
            ShowConfig,
            debug = args.contains("--debug"),
            sequential = bool(args, "--sequential")
          )
        case "validate" :: args => ConsoleCommand(
            Validate,
            debug = args.contains("--debug"),
            sequential = bool(args, "--sequential")
          )
        case _ => failure("Invalid command. Try `run [args]`")
    }
  end parse

end ConsoleCommand

def run(project: Option[String])(using Settings): Result[Unit, String] =
  def doRun(app: Module) = Result {
    val plan         = Plan.compile(Set(app), SubCommand.Run).?
    val initialState = parseCache.?
    val finalResult  = plan.exec(initialState).?
    writeCacheDiff(finalResult, initialState).?
    Tasks.run(app, app.kind.asApplication, finalResult).?
  }

  Result {
    settings.config.modules.values.filter(_.kind.isInstanceOf[ModuleKind.Application]).toList match
      case Nil => failure("No application modules found")
      case apps =>
        project match
          case Some(name) =>
            apps.find(_.name == name) match
              case Some(app) => doRun(app).?
              case None      => failure(s"No application module found for name $name")
          case None =>
            apps match
              case app :: Nil => doRun(app).?
              case _ =>
                failure(
                  s"Multiple application modules found (${apps.map(_.name).mkString(", ")}), please specify one with `run <name>`"
                )
  }
end run

def clean()(using Settings): Result[Unit, String] =
  Result {
    Tasks.clean(settings.config.modules.values.toSet).?
    Shared.clearAndRemoveDir(os.pwd / ".scala-builder").?
  }

def test(opts: Test)(using Settings): Result[Unit, String] =
  Result {
    val mods = settings.config.modules.values.toSet
    val filtered =
      if opts.projects.isEmpty then mods
      else mods.filter(m => opts.projects.contains(m.name))
    val modsMsg      = if filtered.sizeIs == 1 then "module" else "modules"
    val plan         = Plan.compile(filtered, SubCommand.Test).?
    val initialState = parseCache.?
    val finalResult  = plan.exec(initialState).?
    writeCacheDiff(finalResult, initialState).?
    Tasks.test(filtered, finalResult, initial = initialState).?
  }

def repl(opts: Repl)(using Settings): Result[Unit, String] =
  Result {
    settings.config.modules.values.find(_.name == opts.project) match
      case Some(module) =>
        val plan         = Plan.compile(Set(module), SubCommand.Repl).?
        val initialState = parseCache.?
        val finalResult  = plan.exec(initialState).?
        writeCacheDiff(finalResult, initialState).?
        Tasks.repl(module, finalResult).?
      case None => failure(s"Module ${opts.project} not found")
  }

def showConfig()(using Settings): Unit =
  println(write(settings.config, indent = 2))

def validate(): Unit =
  println("config is valid")

def configFile(workspace: Option[os.Path] = None): Result[os.Path, String] =
  val location = workspace.getOrElse(os.pwd) / "builder.toml"
  if os.exists(location) then Result.Success(location)
  else Result.Failure(s"There is no existing config file at $location")

def parseConfig(location: os.Path): Result[Config, String] =
  def readConfig() =
    Result.attempt {
      os.read(location)
    }.resolve {
      case err: IOException => s"error while reading config file: $err"
    }

  Result {
    if os.exists(location) then
      Config.parse(readConfig().?).?
    else
      failure("No builder.toml file found in current directory")
  }

end parseConfig

def parseCache(using Settings): Result[targets.Targets, String] =
  Result.attempt {
    if os.exists(cachePath) then
      reporter.debug(s"found cache at $cachePath")
      read[targets.Targets](os.read(cachePath))
      // TODO: validate against config
    else
      targets.Targets(Map.empty)
  }
    .resolve {
      case err: IOException => s"error while parsing cache: $err"
    }

def writeCacheDiff(project: targets.Targets, initial: targets.Targets)(using
  Settings
): Result[Unit, String] =
  if initial eq project then
    reporter.debug(s"targets had no diff, will not write any updates to the cache.")
    Result.Success(())
  else
    reporter.debug(s"writing cache to $cachePath")
    Result.attempt {
      os.write.over(cachePath, write(project), createFolders = true)
    }
      .resolve {
        case err: IllegalArgumentException => s"error while writing cache: $err"
      }

def execCommand(command: ConsoleCommand): Result[Unit, String] =
  Result {
    val location   = configFile().?
    val settings   = Settings(command.debug, command.sequential, parseConfig(location).?)
    given Settings = settings
    command.sub match
      case Run(project)   => run(project).?
      case Clean          => clean().?
      case opts @ Repl(_) => repl(opts).?
      case opts @ Test(_) => test(opts).?
      case ShowConfig     => showConfig()
      case Validate       => validate()
  }
