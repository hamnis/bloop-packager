package net.hamnaberg.blooppackager

import cats.syntax.all._
import bloop.config.PlatformFiles
import com.monovore.decline.{Command, Opts}

import java.nio.file.Path

object Commands {
  private val defaultDir = PlatformFiles.userDir.resolve(".bloop")

  val config = Opts
    .option[Path]("config", "directory of bloop configuration, defaults to $PWD/.bloop")
    .withDefault(defaultDir)

  val projectOpt = Opts
    .option[String]("project", "Select which bloop project to build, default all")
    .orNone

  val programOpts: Opts[List[Program]] = Opts
    .options[String]("program", "Shell and bat scripts to generate")
    .mapValidated(nel => nel.traverse(Program.parse))
    .orNone
    .map(_.map(_.toList).getOrElse(List.empty[Program]))

  val jarCommand = Command[Cmd]("jar", "Package up a jar from compiled classes and resources") {
    projectOpt.map(projectOpt => Jar(projectOpt))
  }
  val distCommand = Command[Cmd](
    "dist",
    "execute jar transitively, and locate all jar dependencies, put all the dependencies in a lib directory specified at") {
    (projectOpt, programOpts, Opts.option[Path]("path", "Path to where to put ").orNone)
      .mapN(Dist.apply)
  }

  val appCmd = (config.map(GlobalOpts.apply), Opts.subcommands(distCommand, jarCommand)).tupled
}
