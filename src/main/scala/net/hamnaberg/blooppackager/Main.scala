package net.hamnaberg.blooppackager

import com.monovore.decline._

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
        App.run(global, cmd)
      }
      .fold(
        message => {
          Console.err.println(message)
          sys.exit(1)
        },
        identity
      )
  }
}
