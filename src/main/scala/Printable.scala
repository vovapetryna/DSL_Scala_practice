trait Printable

object Printable{
  implicit class printableJson(value: Json) {
    def pPrint: String = {
     value match {
       case Json.Num(d) => d.toString
       case Json.Real(d) => d.toString
       case Json.Str(s) => s.mkString("\"", "", "\"")
       case Json.None => "none"
       case Json.Arr(objs) => objs.map(obj => obj.pPrint).mkString("[", ",", "]")
       case _ => ""
     }
    }
  }
}