sealed trait Json {
  def decodeAs[T](implicit decoder: Decoder[T]): Option[T] =
    decoder.decode(this)
}

object Json {
  final case class Num(value: BigDecimal) extends Json

  final case class Str(value: String) extends Json

  final case class Obj(fields: Map[String, Json]) extends Json

  final case class Arr(objs: List[Json]) extends Json

  final case class Bool(value: Boolean) extends Json

  final case object JNone extends Json

  final case class Delim(value: Char) extends Json

  def convertFromJson[B](x: Json)(implicit decoder: Decoder[B]): Option[B] =
    decoder.decode(x)

  implicit def convertToJson[B](x: B)(implicit encoder: Encoder[B]): Json =
    encoder.encode(x)

  def jRead[B](str: String)(implicit decoder: Decoder[B]): Option[B] = {
    LexicalAnalyzer.build(str) match {
      case Right(tags) =>
        ContextAnalyzer.build(tags) match {
          case Right(jValue) => decoder.decode(jValue)
          case Left(_) => None
        }
      case Left(_) => None
    }
  }
}

trait Encoder[T] {
  def encode(value: T): Json
}

object Encoder extends SimpleEncoder {
  def fromFunction[B](f: B => Json): Encoder[B] = (value: B) => f(value)
}

trait SimpleEncoder {
  implicit val delimEncoder: Encoder[Char] =
    Encoder.fromFunction(value => Json.Delim(value))

  implicit val booleanEncoder: Encoder[Boolean] =
    Encoder.fromFunction(value => Json.Bool(value))

  implicit val noneEncoder: Encoder[Null] =
    Encoder.fromFunction(_ => Json.JNone)

  implicit val numEncoder: Encoder[BigDecimal] =
    Encoder.fromFunction(value => Json.Num(value))

  implicit val strEncoder: Encoder[String] =
    Encoder.fromFunction(value => Json.Str(value))

  implicit def listEncoder[A](implicit encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(list => Json.Arr(list.map(encoder.encode)))
}

trait Decoder[T] {
  def decode(value: Json): Option[T]

  def zip[B](that: Decoder[B]): Decoder[(T, B)] =
    Decoder.fromFunction(json => this.decode(json).zip(that.decode(json)))

  def transform[B](f: T => B): Decoder[B] =
    Decoder.fromFunction(json => this.decode(json).map(f))
}

object Decoder extends SimpleDecoder {
  def fromFunction[B](f: Json => Option[B]): Decoder[B] =
    (value: Json) => f(value)

  def fromPartialFunction[B](f: PartialFunction[Json, B]): Decoder[B] =
    fromFunction(f.lift)
}

trait SimpleDecoder {
  implicit val numDecoder: Decoder[BigDecimal] =
    Decoder.fromPartialFunction { case Json.Num(value) => value }

  implicit val stringDecoder: Decoder[String] =
    Decoder.fromPartialFunction { case Json.Str(value) => value }

  implicit def listDecoder[T](implicit decoder: Decoder[T]): Decoder[List[T]] =
    Decoder.fromFunction {
      case Json.Arr(data) =>
        val parsed = data.map(decoder.decode)
        if (parsed.forall(_.isDefined)) Some(for {
          d <- data
          Some(m) = decoder.decode(d)
        } yield m)
        else None
    }

  implicit def field[T](
      field: String
  )(implicit decoder: Decoder[T]): Decoder[T] =
    Decoder.fromFunction {
      case Json.Obj(obj) => decoder.decode(obj(field))
      case _             => None
    }
}
