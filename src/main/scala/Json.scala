sealed trait Json {
  def decodeAs[T](implicit decoder: Decoder[T]): Option[T] =
    decoder.decode(this)
}

object Json {
  final case class Num(value: Int) extends Json

  final case class Real(value: Double) extends Json

  final case class Str(value: String) extends Json

  final case class Obj(fields: Map[String, Json]) extends Json

  final case class Arr(objs: List[Json]) extends Json

  final case object None extends Json

  def convertFromJson[B](x: Json)(implicit decoder: Decoder[B]): Option[B] =
    decoder.decode(x)

  implicit def convertToJson[B](x: B)(implicit encoder: Encoder[B]): Json =
    encoder.encode(x)
}

trait Encoder[T] {
  def encode(value: T): Json
}

object Encoder extends SimpleEncoder {
  def fromFunction[B](f: B => Json): Encoder[B] = (value: B) => f(value)
}

trait SimpleEncoder {
  implicit val numEncoder: Encoder[Int] =
    Encoder.fromFunction(value => Json.Num(value))

  implicit val realEncoder: Encoder[Double] =
    Encoder.fromFunction(value => Json.Real(value))

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
  implicit val numDecoder: Decoder[Int] =
    Decoder.fromPartialFunction { case Json.Num(value) => value }

  implicit val realDecoder: Decoder[Double] =
    Decoder.fromPartialFunction { case Json.Real(value) => value }

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
