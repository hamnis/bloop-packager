package net.hamnaberg.blooppackager

import cats.syntax.all._
import bloop.config.{Config, ConfigCodecs, PlatformFiles, Tag}

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption, StandardOpenOption}
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import java.util.stream.Collectors
import java.util.zip.ZipEntry
import scala.jdk.CollectionConverters._
import scala.util.Using

import org.rogach.scallop._

object Main {
  private val epochTime = FileTime.fromMillis(0)

  class Config(arguments: Seq[String]) extends ScallopConf(arguments) {
    val config = opt[Path](default = Some(PlatformFiles.userDir.resolve(".bloop")))
    val project = opt[String](default = Some("all"))
    object dist extends Subcommand("dist") {
      val path = opt[Path]()
    }
    addSubcommand(dist)
    verify()
  }

  def main(args: Array[String]): Unit = {
    val conf = new Config(args.toSeq)

    val bloop = conf.config()
    val projectFiles = Files
      .list(bloop)
      .filter(_.toString.endsWith(".json"))
      .collect(Collectors.toList[Path])
      .asScala
      .toList
    projectFiles.traverse(p => ConfigCodecs.read(p)) match {
      case Left(err) => throw err
      case Right(projects) =>
        val filtered = conf.project() match {
          case "all" => projects
          case name => projects.find(_.project.name == name).toList
        }

        filtered
          .flatMap {
            case p if p.project.tags.getOrElse(Nil).contains(Tag.Library) =>
              p.project.platform match {
                case Some(platform: Config.Platform.Jvm) => List(p.project -> platform)
                case _ => Nil
              }
            case _ => Nil
          }
          .foreach { case (project, platform) =>
            if (conf.subcommand.contains(conf.dist)) {
              val distDir = conf.dist.path.getOrElse(project.out)
              Files.createDirectories(distDir)
              val lib = distDir.resolve("lib")
              if (Files.exists(lib)) {
                Files.walk(lib).forEach { p =>
                  if (Files.isRegularFile(p)) {
                    Files.deleteIfExists(p)
                  }
                }
                if (Files.isDirectory(lib)) {
                  Files.deleteIfExists(lib)
                }
              }
              Files.createDirectories(lib)

              val jarFile = lib.resolve(s"${project.name}-jvm.jar")
              jar(project, platform, jarFile)

              project.classpath.filter(Files.isRegularFile(_)).foreach { src =>
                Files.copy(src, lib.resolve(src.getFileName), StandardCopyOption.COPY_ATTRIBUTES)
              }
            } else {
              val jarFile = project.out.resolve(s"${project.name}-jvm.jar")
              jar(project, platform, jarFile)
            }
          }
    }
  }

  def buildManifest(project: Config.Project, platform: Config.Platform.Jvm) = {
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.IMPLEMENTATION_TITLE, project.name)
    platform.mainClass.foreach(cls =>
      manifest.getMainAttributes.put(Attributes.Name.MAIN_CLASS, cls))
    manifest
  }

  def jar(project: Config.Project, platform: Config.Platform.Jvm, file: PlatformFiles.Path) = {
    val classes = project.out.resolve("bloop-bsp-clients-classes/classes-bloop-cli")
    if (Files.notExists(classes)) {
      throw new RuntimeException("You need to run bloop compile before running this")
    }
    val resourceDirectories = project.resources.getOrElse(Nil)
    if (Files.deleteIfExists(file)) {
      Console.err.println(s"Deleted existing $file")
    }
    if (Files.notExists(file)) {
      val manifest = buildManifest(project, platform)
      Using.resource(
        new JarOutputStream(Files.newOutputStream(file, StandardOpenOption.CREATE_NEW), manifest)) {
        os =>
          addFilesToJar(classes, os)
          resourceDirectories.filter(Files.exists(_)).foreach { resourceDir =>
            addFilesToJar(resourceDir, os)
          }
      }
    }
  }

  private def addFilesToJar(root: Path, os: JarOutputStream) =
    Files.walk(root).forEachOrdered { file =>
      val name = root.relativize(file).toString
      if (name.nonEmpty) {
        addJarEntry(os, file, name, Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS))
      }
    }

  private def addJarEntry(os: JarOutputStream, file: Path, name: String, directory: Boolean) = {
    val entry = new ZipEntry(if (directory) s"$name/" else name)
    os.putNextEntry(entry)

    entry.setCreationTime(epochTime)
    entry.setLastModifiedTime(epochTime)
    entry.setLastAccessTime(epochTime)
    if (!directory) {
      entry.setMethod(ZipEntry.DEFLATED)
      entry.setSize(Files.size(file))
      Using.resource(Files.newInputStream(file))(is => is.transferTo(os))
    }
    ()
  }
}
