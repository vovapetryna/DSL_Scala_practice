import Json.{convertFromJson, convertToJson}
import externalDecoder.{JsonToCarDecoder, JsonToPersonDecoder}
import externalEncoder.{personToJsonEncoder, carToJsonEncoder}

object main extends App {
  val emptyCar = Car(0, 0, "")

  val nums: Json = List(2, 3)
  val car: Json = Car(200, 8.5, "AA101BB")
  val parsedNums = convertFromJson[List[Int]](nums)

  val myCar = convertFromJson[Car](car)
  val person: Json =
    Person("Vova", "Petryna", 19, List(myCar.getOrElse(emptyCar)))

  val decodePerson = convertFromJson[Person](person)

  println(nums)
  println(parsedNums)
  println()
  println(car)
  println(myCar)
  println()
  println(person)
  println(decodePerson)

  val exampleNotJson = "{ maxSpeed : 200, fuelConsumption : 8.5, sign : AA101BB}"

  val parser =
    for {
      maxSpeed <- DefaultParsers.KeyInt
      fuel <- DefaultParsers.KeyDouble
      sign <- DefaultParsers.KeyString
    } yield Json.Obj(
      Map(
        maxSpeed._1 -> maxSpeed._2,
        fuel._1 -> fuel._2,
        sign._1 -> sign._2
      )
    )

  println(parser.parse(exampleNotJson))

}
