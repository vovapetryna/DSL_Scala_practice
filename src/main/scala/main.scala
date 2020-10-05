import Json.{convertFromJson, convertToJson}
import externalDecoder.{JsonToCarDecoder, JsonToPersonDecoder}
import externalEncoder.{personToJsonEncoder, carToJsonEncoder}

object main extends App {
  val emptyCar = Car(0, 0, "")

  val nums: Json = List(2, 3)
  val car: Json = Car(200, 8.5, "AA101BB")
  val parsedNums: Option[List[Int]] = convertFromJson[List[Int]](nums)

  val myCar: Option[Car] = convertFromJson[Car](car)
  val person: Json = Person("Vova", "Petryna", 19, List(myCar.getOrElse(emptyCar)))

  val decodePerson: Option[Person] = convertFromJson[Person](person)

  println(nums)
  println(parsedNums)
  println()
  println(car)
  println(myCar)
  println()
  println(person)
  println(decodePerson)

}
