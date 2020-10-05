trait TestClasses

case class Person(
    name: String,
    surname: String,
    age: Int,
    cars: List[Car]
)

case class Car(maxSpeed: Int, fuelConsumption: Double, sign: String)

object externalEncoder {
  implicit val carToJsonEncoder: Encoder[Car] =
    Encoder.fromFunction(car =>
      Json.Obj(
        Map(
          "maxSpeed" -> car.maxSpeed,
          "fuelConsumption" -> car.fuelConsumption,
          "sign" -> car.sign
        )
      )
    )

  implicit val personToJsonEncoder: Encoder[Person] =
    Encoder.fromFunction(person =>
      Json.Obj(
        Map(
          "name" -> person.name,
          "surname" -> person.surname,
          "age" -> person.age,
          "cars" -> person.cars
        )
      )
    )
}

object externalDecoder {
  implicit val JsonToCarDecoder: Decoder[Car] =
    Decoder
      .field[Int]("maxSpeed")
      .zip(Decoder.field[Double]("fuelConsumption"))
      .zip(Decoder.field[String]("sign"))
      .transform[Car] { case ((speed, fuel), sign) => Car(speed, fuel, sign) }

  implicit val JsonToPersonDecoder: Decoder[Person] =
    Decoder
      .field[String]("name")
      .zip(Decoder.field[String]("surname"))
      .zip(Decoder.field[Int]("age"))
      .zip(Decoder.field[List[Car]]("cars"))
      .transform[Person] {
        case (((name, surname), age), cars) => Person(name, surname, age, cars)
      }
}
