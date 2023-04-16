package scala.compose.builder

import errors.*

import collection.mutable
import scala.annotation.tailrec

object ModuleGraph:

  def assemble[T](modules: List[Module]): Result[Map[String, Module], String] =
    Result {
      // prove not cyclic, if cyclic return error, else return None
      val lookup = Map.from(modules.view.map(module => module.name -> module))
      val seen = mutable.Set.empty[String]
      val visiting = mutable.Set.empty[String]
      def visit(node: Module, from: Module | Null): Unit =
        if visiting.contains(node.name) then
          val fromName = Option(from).map(_.name).getOrElse("<unknown>")
          val onMessage = if fromName == node.name then "itself." else s"module '${node.name}'."
          failure(s"module graph is invalid: module '$fromName' has a cyclic dependency on $onMessage")
        else if !seen.contains(node.name) then
          visiting.add(node.name)
          for dep <- node.dependsOn do lookup.get(dep) match
            case Some(module) => visit(module, node)
            case _ => failure(s"module '${node.name}' depends on '$dep' which does not exist.")
          visiting.remove(node.name)
          seen.addOne(node.name)
        else
          ()
      end visit
      lookup.values.foreach(visit(_, null))
      lookup
    }
