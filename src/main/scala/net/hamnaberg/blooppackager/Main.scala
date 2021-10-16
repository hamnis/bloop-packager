package net.hamnaberg.blooppackager

import com.monovore.decline._

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
