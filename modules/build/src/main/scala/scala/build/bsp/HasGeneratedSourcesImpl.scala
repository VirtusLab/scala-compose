package scala.build.bsp

import ch.epfl.scala.{bsp4j => b}

import scala.build.GeneratedSource
import scala.build.input.Inputs
import scala.build.internal.Constants
import scala.build.options.Scope
import scala.collection.mutable

trait HasGeneratedSourcesImpl extends HasGeneratedSources {

  import HasGeneratedSources._

  protected val projectNames     = mutable.Map[Scope, List[ProjectName]]()
  protected val generatedSources = mutable.Map[Scope, GeneratedSources]()

  def targetIds: List[b.BuildTargetIdentifier] =
    projectNames
      .toList
      .sortBy(_._1)
      .flatMap(_._2)
      .flatMap(_.targetUriOpt)
      .map(uri => new b.BuildTargetIdentifier(uri))

  def targetScopeIdOpt(scope: Scope): List[b.BuildTargetIdentifier] =
    projectNames
      .get(scope)
      .toList
      .flatten
      .flatMap(_.targetUriOpt)
      .map(uri => new b.BuildTargetIdentifier(uri))

  def projectScopeNames(scope: Scope): List[(String, b.BuildTargetIdentifier)] =
    projectNames
      .get(scope)
      .toList
      .flatten
      .flatMap(pn => pn.targetUriOpt.map(uri => pn.name -> new b.BuildTargetIdentifier(uri)))

  def resetProjectNames(): Unit =
    projectNames.clear()
  def setProjectName(workspace: os.Path, name: String, scope: Scope): Unit = {
    projectNames(scope) =
      projectNames.get(scope).toList.flatten ++ Seq(ProjectName(workspace, name))
  }

  def newInputs(configDir: Option[os.Path], modules: Seq[Module]): Unit = {
    resetProjectNames()
    modules.foreach { module =>
      val (workspace, mainProjectName, testProjectName) = configDir match {
        case Some(workspace) =>
          (workspace, module.projectName, s"${module.projectName}-test")
        case None =>
          (
            module.inputs.workspace,
            module.inputs.projectName,
            module.inputs.scopeProjectName(Scope.Test)
          )
      }
      setProjectName(workspace, mainProjectName, Scope.Main)
      setProjectName(workspace, testProjectName, Scope.Test)
    }
  }

  def setGeneratedSources(scope: Scope, sources: Seq[GeneratedSource]): Unit = {
    generatedSources(scope) = GeneratedSources(sources)
  }

  protected def targetWorkspaceDirOpt(id: b.BuildTargetIdentifier): Option[String] =
    projectNames.collectFirst {
      case (_, projName) if projName.exists(_.targetUriOpt.contains(id.getUri)) =>
        (projName.head.bloopWorkspace / Constants.workspaceDirName).toIO.toURI.toASCIIString
    }
  protected def targetScopeOpt(id: b.BuildTargetIdentifier): Option[Scope] =
    projectNames.collectFirst {
      case (scope, projName) if projName.exists(_.targetUriOpt.contains(id.getUri)) =>
        scope
    }
  protected def validTarget(id: b.BuildTargetIdentifier): Boolean =
    targetScopeOpt(id).nonEmpty

}
