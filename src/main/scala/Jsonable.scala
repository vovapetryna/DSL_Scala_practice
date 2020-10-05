trait Jsonable[T] {
  def serialize(t: T): Json
}

object Jsonable {
  implicit object IntJsonable extends Jsonable[Int] {
    def serialize(value: Int): Json =
      Json.Num(if (value.isValidInt) value else 0)
  }

  implicit object StringJsonable extends Jsonable[String] {
    def serialize(value: String): Json =
      Json.Str(value)
  }

  implicit object DoubleJsonable extends Jsonable[Double] {
    def serialize(value: Double): Json =
      Json.Real(value)
  }

  implicit object ListJsonable extends Jsonable[List[Json]] {
    def serialize(array: List[Json]): Json =
      Json.Arr(array)
  }
}