package scala.compose.builder

object reporter:
  def processLines(prefix: String, msg: String): String =
    msg.linesIterator.map(prefix + _).mkString("\n")

  inline def debug(inline msg: String)(using Settings): Unit =
    if settings.debug then Console.err.println(processLines("[debug] ", msg))
  inline def info(inline msg: String): Unit =
    Console.err.println(processLines("[info] ", msg))
  inline def error(inline msg: String): Unit =
    Console.err.println(processLines("[error] ", msg))
