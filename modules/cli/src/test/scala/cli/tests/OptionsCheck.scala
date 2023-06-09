package scala.cli.tests

import scala.cli.ScalaCliCommands

class OptionsCheck extends munit.FunSuite {

  for (
    command <-
      (new ScalaCliCommands("scala-cli", "scala-cli", "Scala CLI", isSipScala = false)).commands
  )
    test(s"No duplicated options in ${command.names.head.mkString(" ")}") {
      command.ensureNoDuplicates()
    }

}
