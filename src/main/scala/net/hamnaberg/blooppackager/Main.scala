package net.hamnaberg.blooppackager

import bloop.config.Config.Platform
import cats.syntax.all._
import bloop.config.{Config, ConfigCodecs, PlatformFiles, Tag}
import com.monovore.decline._

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.nio.file._
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import java.util.stream.Collectors
import java.util.zip.ZipEntry
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.math.Ordered.orderingToOrdered
import scala.util.Using

final case class GlobalOpts(config: Path)

sealed trait Cmd {
  def project: Option[String]
}

final case class Jar(project: Option[String]) extends Cmd
final case class Dist(project: Option[String], path: Option[Path]) extends Cmd

sealed trait Code
object Code {
  case object Success extends Code
  final case class Error(message: String) extends Code
}

object Main {
  private val mainCmd = Command(name = "bloop-packager", "Bloop Packager", helpFlag = true)(
    Commands.appCmd
  )

  def main(args: Array[String]): Unit = {
    val arguments = PlatformApp.ambientArgs.getOrElse(args.toList)
    val parsed = mainCmd.parse(arguments, sys.env).map { case (global, cmd) =>
      App.run(global, cmd)
    }
    parsed match {
      case Left(help) =>
        Console.err.println(help)
        sys.exit(1)
      case Right(code) =>
        code match {
          case Code.Success => ()
          case Code.Error(message) =>
            Console.err.println(message)
            sys.exit(1)
        }
    }
  }
}

object Commands {
  private val defaultDir = PlatformFiles.userDir.resolve(".bloop")

  val config = Opts
    .option[Path]("config", "directory of bloop configuration, defaults to $PWD/.bloop")
    .withDefault(defaultDir)

  val projectOpt = Opts
    .option[String]("project", "Select which bloop project to build, default all")
    .orNone

  val jarCommand = Command[Cmd]("jar", "Package up a jar from compiled classes and resources") {
    projectOpt.map(projectOpt => Jar(projectOpt))
  }
  val distCommand = Command[Cmd](
    "dist",
    "execute jar transitively, and locate all jar dependencies, put all the dependencies in a lib directory specified at") {
    (projectOpt, Opts.option[Path]("path", "Path to where to put ").orNone)
      .mapN((projectOpt, pathOpt) => Dist(projectOpt, pathOpt))
  }

  val appCmd = (config.map(GlobalOpts.apply), Opts.subcommands(distCommand, jarCommand)).tupled
}

object App {
  private val epochTime = FileTime.fromMillis(0)

  def run(global: GlobalOpts, cmd: Cmd): Code =
    if (Files.notExists(global.config)) {
      Code.Error(s"${global.config} does not exist")
    } else {
      val projectFiles = Files
        .list(global.config)
        .filter(_.toString.endsWith(".json"))
        .collect(Collectors.toList[Path])
        .asScala
        .toList
      projectFiles.traverse(p => ConfigCodecs.read(p)) match {
        case Left(err) =>
          Code.Error(s"Unable to parse bloop project files, ${err.getMessage}")
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
                case Dist(_, distPath) =>
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
                  println(distDir)
              }
            }
          Code.Success
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
    val internal = project.out.resolve("bloop-internal-classes")
    if (Files.exists(internal)) {
      val previousPath = project.out.resolve(".previous-classes-directory")
      val resources = project.resources.getOrElse(Nil)
      val resourceLastChange: Option[FileTime] = resources
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

      val previous =
        Option.when(Files.exists(previousPath))(Files.readString(previousPath)).map(Paths.get(_))
      val classesDir =
        Files
          .list(internal)
          .filter(_.getFileName.toString.startsWith("classes-bloop-cli"))
          .findFirst()
          .toScala

      classesDir.foreach { classes =>
        val nonEmptyDir = Files.list(classes).findFirst().isPresent //detect if we are empty

        if (!previous.contains(classes) && nonEmptyDir) {
          Files.writeString(
            previousPath,
            classes.toString,
            StandardCharsets.UTF_8,
            StandardOpenOption.WRITE,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING)
          buildJar(project, platform, jarFile, classes)
        } else if (Files.exists(jarFile) && resourceLastChange.exists(change =>
            change > Files.getLastModifiedTime(jarFile))) {
          buildJar(project, platform, jarFile, classes)
        }
      }
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
