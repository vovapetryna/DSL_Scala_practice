import Json.convertToJson

import scala.annotation.tailrec
import scala.util.matching.Regex

object JSyntax {
  val QUOTE = "\""

  val delimiterRegex: Regex = """([{} ,:\[\]])(.*)""".r
  val nRegex: Regex = """([+-]?\d+\.*\d*)(.*)""".r
  val boolRegex: Regex = """(true|false)(.*)""".r
}

class JAnalyzer[T](val build: String => Either[String, (T, String)])

object JAnalyzer {
  def apply[T](build: String => Either[String, (T, String)]): JAnalyzer[T] =
    new JAnalyzer(build)
}

object analyzer {
  def string_lex: JAnalyzer[Json] =
    JAnalyzer[Json] { (str: String) =>
      {
        if (str.startsWith(JSyntax.QUOTE)) {
          str.drop(1).indexOf(JSyntax.QUOTE) match {
            case -1     => Left("Expected end-of-string quote")
            case p: Int => Right((str.slice(1, p + 1), str.drop(1 + p + 1)))
          }
        } else Left("Not a string")
      }
    }

  def lex_number: JAnalyzer[Json] = JAnalyzer[Json] {
      case JSyntax.nRegex(number, rest) => Right((BigDecimal(number), rest))
      case _ => Left("Not a number")
    }

  def lex_bool: JAnalyzer[Json] = JAnalyzer[Json] {
      case JSyntax.boolRegex(bool, rest) => Right((bool.toBoolean, rest))
      case _ => Left("Not a boolean literal")
    }

  def lex_null: JAnalyzer[Json] = JAnalyzer[Json] { (str: String) =>
      if (str.startsWith("null")) Right((Json.JNone, str.drop(4)))
      else Left("Not a null literal")
    }

  def lex_separator: JAnalyzer[Json] = JAnalyzer[Json] {
      case JSyntax.delimiterRegex(" ", _) => Left("empty delimiter")
      case JSyntax.delimiterRegex(value, rest) => Right((value(0), rest))
      case _  => Left("Delimiter not found")
    }

  val all = List(string_lex, lex_number, lex_bool, lex_null, lex_separator)
}

object LexicalAnalyzer {
  def build(str: String):Either[String, List[Json]] = {
    @tailrec
    def auxiliaryBuild(str: String, tokens: List[Json]): Either[String, List[Json]] = {
      if (str.isEmpty) Right(tokens.reverse) else {
        val lex_step = for {
          Right((value, rest)) <- analyzer.all.map(f => f.build(str))
        } yield (value, rest)

        lex_step match {
          case Nil => auxiliaryBuild(str.drop(1), tokens)
          case (value, rest) :: _ => auxiliaryBuild(rest, value :: tokens)
        }
      }
    }
    auxiliaryBuild(str.filter(_ >= ' '), Nil)
  }
}
