package ru.avhaliullin.whatever.common

/**
  * @author avhaliullin
  */
sealed trait PrettyPrint {
  def prepend(s: String): PrettyPrint

  def append(s: String): PrettyPrint
}

object PrettyPrint {

  case class Literal(s: String) extends PrettyPrint {
    override def prepend(p: String): PrettyPrint = Literal(p + s)

    override def append(a: String): PrettyPrint = Literal(s + a)
  }

  case class Complex(start: String, end: String, children: Seq[PrettyPrint]) extends PrettyPrint {
    override def prepend(p: String): PrettyPrint = copy(start = p + start)

    override def append(a: String): PrettyPrint = copy(end = end + a)
  }

  case class Printer(indent: String = "  ") {
    private def _print(pp: PrettyPrint): Seq[String] = {
      pp match {
        case PrettyPrint.Literal(s) => Seq(s)
        case PrettyPrint.Complex(start, end, content) =>
          val lines = content.flatMap(_print)
          if (lines.size > 1) {
            start +: lines.map(indent + _) :+ end
          } else {
            Seq(start + lines.mkString + end)
          }
      }
    }

    def print(pp: PrettyPrint): String = {
      _print(pp).mkString("\n")
    }
  }

}
