package scala.compose.commands.bsp

import caseapp.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import scala.build.EitherCps.{either, value}
import scala.build.*
import scala.build.bsp.{BspReloadableOptions, BspThreads}
import scala.build.errors.BuildException
import scala.build.input.Inputs
import scala.build.internal.CustomCodeWrapper
import scala.build.options.BuildOptions
import scala.cli.CurrentParams
import scala.cli.commands.ScalaCommand
import scala.cli.commands.publish.ConfigUtil.*
import scala.cli.commands.shared.SharedOptions
import scala.cli.config.{ConfigDb, Keys}
import scala.compose.builder.*
import scala.compose.builder.targets.{Target, TargetKind}
import scala.compose.builder.errors.*
import scala.concurrent.Await
import scala.concurrent.duration.Duration
object Bsp extends ScalaCommand[BspOptions] {
  override def hidden                  = true
  override def scalaSpecificationLevel = SpecificationLevel.IMPLEMENTATION
  private def latestSharedOptions(options: BspOptions): SharedOptions =
    options.jsonOptions.map { optionsPath =>
      val content = os.read.bytes(os.Path(optionsPath, os.pwd))
      readFromArray(content)(SharedOptions.jsonCodec)
    }.getOrElse(options.shared)
  override def sharedOptions(options: BspOptions): Option[SharedOptions] =
    Option(latestSharedOptions(options))

  // not reusing buildOptions here, since they should be reloaded live instead
  override def runCommand(options: BspOptions, args: RemainingArgs, logger: Logger): Unit = {

    val getSharedOptions: () => SharedOptions = () => latestSharedOptions(options)
    val configDir                             = os.Path(options.confDir, Os.pwd)

    val argsToInputs: Seq[String] => Either[BuildException, Inputs] =
      argsSeq =>
        either {
          val sharedOptions = getSharedOptions()
          val initialInputs = value(sharedOptions.inputs(argsSeq, () => Inputs.default()))

          if (sharedOptions.logging.verbosity >= 3)
            pprint.err.log(initialInputs)

          val buildOptions0    = buildOptions(sharedOptions)
          val latestLogger     = sharedOptions.logging.logger
          val persistentLogger = new PersistentDiagnosticLogger(latestLogger)

          val allInputs =
            CrossSources.forInputs(
              initialInputs,
              Sources.defaultPreprocessors(
                buildOptions0.scriptOptions.codeWrapper.getOrElse(CustomCodeWrapper),
                buildOptions0.archiveCache,
                buildOptions0.internal.javaClassNameVersionOpt,
                () => buildOptions0.javaHome().value.javaCommand
              ),
              persistentLogger,
              buildOptions0.suppressWarningOptions
            ).map(_._2).getOrElse(initialInputs)

          Build.updateInputs(allInputs, buildOptions0)
        }

    val bspReloadableOptionsReference = BspReloadableOptions.Reference { () =>
      val sharedOptions = getSharedOptions()
      BspReloadableOptions(
        buildOptions = buildOptions(sharedOptions),
        bloopRifleConfig = sharedOptions.bloopRifleConfig().orExit(sharedOptions.logger),
        logger = sharedOptions.logging.logger,
        verbosity = sharedOptions.logging.verbosity
      )
    }

    val actionableDiagnostics =
      options.shared.logging.verbosityOptions.actions

    class ScalaComposeException(msg: String) extends BuildException(msg)

    extension [V](res: Result[V, String])
      private def asBuildFailure = res.toEither.left.map(ScalaComposeException(_))

    val argsToInputsModule: Seq[String] => Either[BuildException, Seq[scala.build.bsp.Module]] =
      _ =>
        either {
          val configLocation = value(configFile(Some(configDir)).asBuildFailure)
          val config: Config = value(parseConfig(configLocation).asBuildFailure)
          val ms =
            for
              (_, m) <- config.modules.iterator
            yield
              val inputsRaw = Seq((configDir / m.root).toString())

              val inputs = value(argsToInputs(inputsRaw))

              scala.build.bsp.Module(
                inputs,
                inputs.sourceHash(),
                m.name,
                m.dependsOn,
                m.platforms.map(_.toString),
                m.resourceGenerators.flatMap {
                  case ResourceGenerator.Copy(Target(module, TargetKind.Package), dest) =>
                    Some(module -> os.RelPath(dest))
                  case _ => None
                }
              )
            end for
          end ms
          ms.toSeq
        }

    val modules = argsToInputsModule(Seq()).orExit(logger)

    BspThreads.withThreads { threads =>
      val bsp = scala.build.bsp.Bsp.create(
        argsToInputsModule,
        bspReloadableOptionsReference,
        threads,
        System.in,
        System.out,
        actionableDiagnostics,
        Some(configDir)
      )

      try {
        val doneFuture = bsp.run(modules)
        Await.result(doneFuture, Duration.Inf)
      }
      finally bsp.shutdown()
    }
  }

  private def buildOptions(sharedOptions: SharedOptions): BuildOptions = {
    val logger      = sharedOptions.logger
    val baseOptions = sharedOptions.buildOptions().orExit(logger)
    baseOptions.copy(
      classPathOptions = baseOptions.classPathOptions.copy(
        fetchSources = baseOptions.classPathOptions.fetchSources.orElse(Some(true))
      ),
      scalaOptions = baseOptions.scalaOptions.copy(
        generateSemanticDbs = baseOptions.scalaOptions.generateSemanticDbs.orElse(Some(true))
      ),
      notForBloopOptions = baseOptions.notForBloopOptions.copy(
        addRunnerDependencyOpt =
          baseOptions.notForBloopOptions.addRunnerDependencyOpt.orElse(Some(false))
      )
    )
  }
}
