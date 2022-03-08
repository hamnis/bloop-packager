package net.hamnaberg.blooppackager

import bloop.config.Config.Platform
import bloop.config.{Config, ConfigCodecs, Tag}

import cats.syntax.all._

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import java.util.stream.Collectors
import java.util.zip.ZipEntry
import scala.util.Using

import scala.jdk.CollectionConverters._

import scala.math.Ordered.orderingToOrdered

object App {
  private val epochTime = FileTime.fromMillis(0)

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
          val candidates = parsedProjects
            .flatMap {
              case p if p.project.tags.getOrElse(Nil).contains(Tag.Library) =>
                p.project.platform match {
                  case Some(platform: Config.Platform.Jvm) => List(p.project -> platform)
                  case _ => Nil
                }
              case _ => Nil
            }

          val filtered = cmd.project match {
            case Some(name) => candidates.find(_._1.name == name).toList
            case None => candidates
          }

          val dependencyLookup = candidates.map(t => t._1.classesDir -> t).toMap

          filtered
            .foreach { case (project, platform) =>
              cmd match {
                case Jar(_) =>
                  val maybeJar = jar(project, platform)
                  maybeJar.foreach(println)
                case Dist(_, programs, distPath) =>
                  val distDir =
                    distPath.map(_.resolve(project.name)).getOrElse(project.out.resolve("dist"))
                  Files.createDirectories(distDir)
                  val lib = distDir.resolve("lib")
                  deleteDirectory(lib)
                  Files.createDirectories(lib)

                  val jarFiles = dependenciesFor(project, platform, dependencyLookup).distinct

                  jarFiles.foreach { src =>
                    Files.copy(
                      src,
                      lib.resolve(src.getFileName),
                      StandardCopyOption.COPY_ATTRIBUTES)
                  }
                  if (programs.nonEmpty) {
                    val bin = distDir.resolve("bin")
                    deleteDirectory(bin)
                    Files.createDirectories(bin)
                    Scripts.writeScripts(bin, "", programs)
                  }

                  println(distDir)
              }
            }
          Right(())
      }
    }

  private def deleteDirectory(dir: Path): Unit =
    if (Files.exists(dir)) {
      Files.walkFileTree(
        dir,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
            Files.deleteIfExists(dir)
            FileVisitResult.CONTINUE
          }
        }
      )
    }

  def dependenciesFor(
      project: Config.Project,
      platform: Config.Platform.Jvm,
      lookup: Map[Path, (Config.Project, Config.Platform.Jvm)]): List[Path] = {
    val (dirs, jars) = project.classpath.partition(Files.isDirectory(_))
    val mayBeJar = jar(project, platform)
    mayBeJar.toList ::: jars ::: dirs
      .flatMap(dir => lookup.get(dir).toList)
      .flatMap { case (dependantProject, platform) =>
        dependenciesFor(dependantProject, platform, lookup)
      }
  }

  def buildManifest(project: Config.Project, platform: Config.Platform.Jvm) = {
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.IMPLEMENTATION_TITLE, project.name)
    platform.mainClass.foreach(cls =>
      manifest.getMainAttributes.put(Attributes.Name.MAIN_CLASS, cls))
    manifest
  }

  def jar(project: Config.Project, platform: Config.Platform.Jvm): Option[Path] = {

    val jarFile = project.out.resolve(s"${project.name}-jvm.jar")
    val classes = Some(project.classesDir)
      .filterNot(
        Files
          .list(_)
          .iterator()
          .asScala
          .forall(p => p.toString.endsWith("bloop-internal-classes")))
      .getOrElse(project.out.resolve("bloop-bsp-clients-classes").resolve("classes-bloop-cli"))
    val resourceLastChange: Option[FileTime] = project.resources
      .getOrElse(Nil)
      .filter(Files.exists(_))
      .flatMap(p =>
        Files
          .walk(p)
          .filter(Files.isRegularFile(_))
          .map(file => Files.getLastModifiedTime(file, LinkOption.NOFOLLOW_LINKS))
          .collect(Collectors.toList[FileTime])
          .asScala
          .maxOption)
      .maxOption

    if (Files.exists(classes)) {
      buildJar(project, platform, jarFile, classes)
    } else if (Files.exists(jarFile) && resourceLastChange.exists(change =>
        change > Files.getLastModifiedTime(jarFile))) {
      buildJar(project, platform, jarFile, classes)
    }

    Option.when(Files.exists(jarFile))(jarFile)
  }

  private def buildJar(
      project: Config.Project,
      platform: Platform.Jvm,
      file: Path,
      classes: Path) = {
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
      if (name != "bloop-internal-classes" && name.nonEmpty) {
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
