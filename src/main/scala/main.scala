import Json.jRead
import externalDecoder.JsonToPersonDecoder

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

  val obj = jRead[Person](PersonObjJson)

  println(obj)
}
