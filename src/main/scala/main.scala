import Json.{convertFromJson, convertToJson}
import externalDecoder.{JsonToCarDecoder, JsonToPersonDecoder}
import externalEncoder.{personToJsonEncoder, carToJsonEncoder}

object main extends App {

  val carArrJson =
    """[
      |{
      |  "maxSpeed" : 250,
      |  "sign" : "AA0000BB"
      |},
      |{
      |  "maxSpeed" : 120,
      |  "sign" : "AA0001BB"
      |}
      |]
      |""".stripMargin

  val PersonObjJson =
    """
      |{
      |  "name" : "TestName",
      |  "surname" : "TestSurname",
      |  "age" : 100,
      |  "cars" : """.stripMargin + carArrJson +
      """
      |}
      |""".stripMargin

  val Right(analyzed) = LexicalAnalyzer.build(PersonObjJson)

  val parsed = ContextAnalyzer
    .build(analyzed)

  println(parsed)
}
