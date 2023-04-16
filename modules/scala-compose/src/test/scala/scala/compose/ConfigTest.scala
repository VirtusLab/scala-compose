package scala.compose

import scala.compose.builder.errors.Result
import scala.compose.builder.targets.*
import scala.compose.builder.ScalaCommand.SubCommand

import scala.compose.builder.*

class ConfigTest extends munit.FunSuite {

  // TODO: - first the app reads the config into a Config data structure
  //       - then for each module, read the project json from Scala CLI
  //       - then zip the two together to get ModuleSettings, which adds in info from the project json
  //       - then product the target graph from the ModuleSettings

  val exampleFullStackAppConf = """
  scalaVersion = "3.2.2"

  [modules.webpage]
  platforms = ["scala-js"]
  kind = "application"
  mainClass = "example.start"
  dependsOn = ["core"]

  [modules.webserver]
  kind = "application"
  mainClass = "example.WebServer"
  dependsOn = ["core"]
  resourceGenerators = [{ module = "webpage", dest = "assets/main.js" }]

  [modules.core]
  platforms = ["jvm", "scala-js"]
  """

  extension [T](result: Result[T, String])
    def orFail: T = result match
      case Result.Success(value) => value
      case Result.Failure(err)   => fail(s"failed result: $err")

  test("parse config from valid toml") {

    val config = Config.parse(exampleFullStackAppConf).orFail

    assertEquals(
      config,
      Config(
        scalaVersion = Some("3.2.2"),
        modules = Map(
          "webpage" -> Module(
            name = "webpage",
            root = "webpage",
            kind = ModuleKind.Application(mainClass = Some("example.start")),
            platforms = List(PlatformKind.`scala-js`),
            dependsOn = List("core")
          ),
          "webserver" -> Module(
            name = "webserver",
            root = "webserver",
            kind = ModuleKind.Application(mainClass = Some("example.WebServer")),
            dependsOn = List("core"),
            resourceGenerators = List(
              ResourceGenerator.Copy(
                target = Target(
                  module = "webpage",
                  kind = TargetKind.Package
                ),
                dest = "assets/main.js"
              )
            )
          ),
          "core" -> Module(
            name = "core",
            root = "core",
            kind = ModuleKind.Library,
            platforms = List(PlatformKind.jvm, PlatformKind.`scala-js`),
            dependsOn = Nil
          )
        )
      )
    )
  }

  def stageTest(name: munit.TestOptions)(
    rawConfig: String,
    command: SubCommand,
    targets: Seq[String],
    expected: List[List[String]]
  )(using munit.Location) =
    test(name) {
      val config = Config.parse(rawConfig).orFail

      val targetGraph =
        TargetGraph.compile(
          graph = config.modules,
          targetModules =
            if targets.isEmpty then config.modules.values.toList
            else targets.map(config.modules).toList,
          command
        ).orFail

      val targetNames = targetGraph.stages.map(_.map(_.show))

      assertEquals(targetNames, expected)
    }

  stageTest("sort module deps into stages [full-stack app, run]")(
    rawConfig = exampleFullStackAppConf,
    command = SubCommand.Run,
    targets = Seq("webserver"),
    expected = List(
      List("core:main:scala-js"),
      List("webpage:package", "core:main:jvm"),
      List("webserver:copy[webpage:package]"),
      List("webserver:runner")
    )
  )

  stageTest("sort module deps into stages [full-stack app, repl]")(
    rawConfig = exampleFullStackAppConf,
    command = SubCommand.Repl,
    targets = Seq("webserver"),
    expected = List(
      List("core:main:scala-js"),
      List("webpage:package", "core:main:jvm"),
      List("webserver:copy[webpage:package]"),
      List("webserver:main:jvm")
    )
  )

  val diamondAppConf = """
  [modules.bottom]

  [modules.left]
  dependsOn = ["bottom"]

  [modules.right]
  dependsOn = ["bottom"]

  [modules.top]
  dependsOn = ["left", "right"]
  """

  stageTest("sort module deps into stages [diamond]")(
    rawConfig = diamondAppConf,
    command = SubCommand.Repl,
    targets = Seq(),
    expected = List(
      List("bottom:main:jvm"),
      List("left:main:jvm", "right:main:jvm"),
      List("top:main:jvm")
    )
  )

  stageTest("sort module deps into stages [diamond, filtered]")(
    rawConfig = diamondAppConf,
    command = SubCommand.Repl,
    targets = Seq("right"),
    expected = List(
      List("bottom:main:jvm"),
      List("right:main:jvm")
    )
  )

  val chainAppConf = """
  [modules.D]

  [modules.C]
  dependsOn = ["D"]

  [modules.B]
  dependsOn = ["C"]

  [modules.A]
  dependsOn = ["B"]
  """

  stageTest("sort module deps into stages [chain]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq(),
    expected = List(
      List("D:main:jvm"),
      List("C:main:jvm"),
      List("B:main:jvm"),
      List("A:main:jvm")
    )
  )

  stageTest("sort module deps into stages [chain, filtered-D]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("D"),
    expected = List(
      List("D:main:jvm")
    )
  )

  stageTest("sort module deps into stages [chain, filtered-C]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("C"),
    expected = List(
      List("D:main:jvm"),
      List("C:main:jvm")
    )
  )

  stageTest("sort module deps into stages [chain, filtered-B]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("B"),
    expected = List(
      List("D:main:jvm"),
      List("C:main:jvm"),
      List("B:main:jvm")
    )
  )

  stageTest("sort module deps into stages [chain, filtered-A]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("A"),
    expected = List(
      List("D:main:jvm"),
      List("C:main:jvm"),
      List("B:main:jvm"),
      List("A:main:jvm")
    )
  )

  val forkedAppConf = """
  [modules.common]

  [modules.libA]
  dependsOn = ["common"]

  [modules.libB]
  dependsOn = ["common"]

  [modules.topA]
  dependsOn = ["libA"]

  [modules.topB]
  dependsOn = ["libB"]
  """

  stageTest("sort module deps into stages [forked]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq(),
    expected = List(
      List("common:main:jvm"),
      List("libA:main:jvm", "libB:main:jvm"),
      List("topA:main:jvm", "topB:main:jvm")
    )
  )

  stageTest("sort module deps into stages [forked, filtered-topA]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq("topA"),
    expected = List(
      List("common:main:jvm"),
      List("libA:main:jvm"),
      List("topA:main:jvm")
    )
  )

  stageTest("sort module deps into stages [forked, filtered-topA+topB]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq("topA", "topB"),
    expected = List(
      List("common:main:jvm"),
      List("libA:main:jvm", "libB:main:jvm"),
      List("topA:main:jvm", "topB:main:jvm")
    )
  )

  stageTest("sort module deps into stages [forked, filtered-libA+libB]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq("libA", "libB"),
    expected = List(
      List("common:main:jvm"),
      List("libA:main:jvm", "libB:main:jvm")
    )
  )

}
