package net.hamnaberg.blooppackager

import cats.syntax.all._
import bloop.config.ConfigCodecs
import com.monovore.decline._

import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import scala.jdk.CollectionConverters._

object Main {
  private val mainCmd = Command(name = "bloop-packager", "Bloop Packager", helpFlag = true)(
    Commands.appCmd
  )

  def main(args: Array[String]): Unit = {
    val arguments = PlatformApp.ambientArgs.getOrElse(args.toList)
    mainCmd
      .parse(arguments, sys.env)
      .left
      .map(_.toString())
      .flatMap { case (global, cmd) =>
        run(global, cmd)
      }
      .fold(
        message => {
          Console.err.println(message)
          sys.exit(1)
        },
        identity
      )
  }

  def run(global: GlobalOpts, cmd: Cmd): Either[String, Unit] =
    if (Files.notExists(global.config)) {
      Left(s"${global.config} does not exist")
    } else {
      val projectFiles = Files
        .list(global.config)
        .filter(_.toString.endsWith(".json"))
        .collect(Collectors.toList[Path])
        .asScala
        .toList
      projectFiles.traverse(p => ConfigCodecs.read(p)) match {
        case Left(err) =>
          Left(s"Unable to parse bloop project files, ${err.getMessage}")
        case Right(parsedProjects) =>
          App.runWithProjects(parsedProjects.map(_.project), cmd).foreach(println)
          Right(())
      }
    }
}
