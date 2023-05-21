package scala.compose

import scala.build.options.PackageType
import scala.build.options.Platform
import scala.build.Logger
import scala.build.errors.BuildException
import scala.build.Build
import scala.compose.builder.targets.Target
import scala.compose.builder.targets.TargetKind
import scala.compose.builder.ResourceGenerator
import scala.compose.builder.Config
import scala.compose.builder.PlatformKind
import scala.compose.builder.errors.Result
import scala.build.EitherCps.{either, value}
import scala.build.input.Inputs
import scala.cli.commands.util.BuildCommandHelpers

object BuildModules {

  class ScalaComposeException(msg: String) extends BuildException(msg)

  extension [V](res: Result[V, String])
    def asBuildFailure = res.toEither.left.map(ScalaComposeException(_))

  def readModules(
    readInputs: Seq[String] => Either[BuildException, Inputs],
    config: Config,
    configDir: os.Path
  ): Either[BuildException, Seq[Build.BuildModule]] = either {
    (for (_, m) <- config.modules yield
      val rawArgs   = Seq((configDir / os.RelPath(m.root)).toString())
      val inputs    = value(readInputs(rawArgs))
      val platforms = m.platforms.map(p => Platform.parse(Platform.normalize(p.toString)).get)

      val doPackage = (pkgType: PackageType, fromModule: String, nuDest: os.RelPath) =>
        (logger: Logger, build: Build.Successful) =>
          either {
            import scala.cli.commands.package0.Package
            import scala.cli.commands.shared.MainClassOptions

            val packageDestDir = Build.packagedRootDir(configDir, fromModule)

            os.makeDir.all(packageDestDir)

            val packageDestPath = (pkgType: @unchecked) match
              case PackageType.Js => packageDestDir / "main.js"

            val _ = value(Package.doPackage0(
              logger = logger,
              outputOpt = Some(packageDestPath.toString),
              force = true,
              forcedPackageTypeOpt = Some(pkgType),
              build = build,
              extraArgs = Nil,
              expectedModifyEpochSecondOpt = None,
              allowTerminate = false,
              mainClassOptions = MainClassOptions()
            ))

            val finalDest = Build.resourcesRootDir(configDir, m.name) / nuDest

            os.copy(
              packageDestPath,
              finalDest,
              createFolders = true,
              replaceExisting = true
            )

            ()
          }

      Build.BuildModule(
        inputs,
        m.name,
        platforms,
        m.dependsOn,
        resourceGenerators = m.resourceGenerators.flatMap {
          case ResourceGenerator.Copy(Target(module, TargetKind.Package), dest) =>
            val dest0      = os.RelPath(dest)
            val fromModule = config.modules(module)

            val doUpdate = (logger: Logger) =>
              either {
                !os.exists(Build.resourcesRootDir(configDir, m.name) / dest0)
              }

            fromModule.platforms match
              case Seq(PlatformKind.`scala-js`) =>
                Some(Build.ResourceGenerator(
                  module,
                  doUpdate,
                  doPackage(PackageType.Js, module, dest0)
                ))
              case _ => None
          case _ => None
        }
      )
    ).toSeq
  }
}
