import Json.{convertFromJson, convertToJson}
import externalDecoder.{JsonToCarDecoder, JsonToPersonDecoder}
import externalEncoder.{personToJsonEncoder, carToJsonEncoder}

object main extends App {
  val emptyCar = Car(0, "")

  val car: Json = Car(200, "AA101BB")

  val myCar = convertFromJson[Car](car)
  val person: Json =
    Person("Vova", "Petryna", 19, List(myCar.getOrElse(emptyCar)))

  val decodePerson = convertFromJson[Person](person)

  val parsedNumber = ".0"
  println(parsedNumber)

  val analyzed = analyzer.string_lex.build("\"hello\" another text")
  println(analyzed)

  val lixyTest = LexicalAnalyzer
    .build("{ \"vova\" : \"test\", \"petryna\" : 123.0, \"boolean\" : true, \"nullable\" : null}")

  println(lixyTest)
}
