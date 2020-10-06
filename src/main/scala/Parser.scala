class JParse[T, Src](private val p: Src => (T, Src)) {
  def flatMap[M](f: T => JParse[M, Src]): JParse[M, Src] =
    JParse { src =>
      val (word, rest) = p(src)
      f(word).p(rest)
    }
  def map[M](f: T => M): JParse[M, Src] =
    JParse { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }
  def parse(src: Src): T = p(src)._1
}

object JParse {
  def apply[T, Src](f: Src => (T, Src)) =
    new JParse[T, Src](f)
}

trait DefaultParsers

object DefaultParsers {
  val separator = ','
  val filedSeparator = ':'

  def StringField: JParse[String, String] =
    JParse[String, String] { str =>
      val idx = str.indexOf(separator)
      if (idx > -1)
        (str.substring(1, idx), "{" + str.substring(idx + 1))
      else (str.substring(1, str.length - 1), "")
    }

  def KeyString: JParse[(String, String), String] =
    DefaultParsers.StringField.map(str => (str.substring(0, str.indexOf(filedSeparator)).trim,
        str.substring(str.indexOf(filedSeparator)+1, str.length).trim))

  def KeyInt: JParse[(String, Int), String] =
    DefaultParsers.KeyString.map(d => (d._1, d._2.toInt))

  def KeyDouble: JParse[(String, Double), String] =
    DefaultParsers.KeyString.map(d => (d._1, d._2.toDouble))
}
