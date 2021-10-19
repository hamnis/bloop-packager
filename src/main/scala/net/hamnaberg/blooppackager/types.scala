package net.hamnaberg.blooppackager

import java.nio.file.Path
import cats.syntax.all._

final case class Program(name: String, mainClass: String)
object Program {
  private val R = "([\\w_-]+):([\\w_\\-.]+)".r
  def parse(parse: String) = parse match {
    case R(name, className) => Program(name, className).validNel
    case s => s"'$s' was not a valid program definition expected to match ${R.pattern}".invalidNel
  }
}

final case class GlobalOpts(config: Path)

sealed trait Cmd {
  def project: Option[String]
}

final case class Jar(project: Option[String]) extends Cmd
final case class Dist(project: Option[String], programs: List[Program], path: Option[Path])
    extends Cmd
