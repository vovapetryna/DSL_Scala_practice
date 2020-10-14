import Json.{Obj, Str, None, Num, Delim, Bool, Arr}

object ContextAnalyzer {
  def build(tokens: List[Json]): Either[String, Json] = {
    tokens match {
      case Delim('{') :: tail =>
        buildObj(tail, Obj(Map())) match {
          case Right((newObj, Nil)) => Right(newObj)
          case Left(er) => Left(er)
          case _ => Left("Error object parsing more then one detected")
        }
      case Delim('[') :: tail =>
        buildArr(tail, Arr(List())) match {
          case Right((newArr, Nil)) => Right(newArr)
          case Left(er) => Left(er)
          case _ => Left("Error array parsing more then one detected")
        }
      case _ => Left("Json must be an object or an array")
    }
  }

  def buildObj(tokens: List[Json], curObj: Obj): Either[String, (Json, List[Json])] =
    tokens match {
      case Delim('{') :: tail => buildObj(tail, Obj(Map()))
      case Delim('}') :: tail => Right((curObj, tail))
      case Delim(',') :: tail => buildObj(tail, curObj)
      case Str(field) :: Delim(':') :: (value: Str) :: tail =>
        buildObj(tail, Obj(curObj.fields + (field -> value)))
      case Str(field) :: Delim(':') :: (value: Num) :: tail =>
        buildObj(tail, Json.Obj(curObj.fields + (field -> value)))
      case Str(field) :: Delim(':') :: (value: Bool) :: tail =>
        buildObj(tail, Obj(curObj.fields + (field -> value)))
      case Str(field) :: Delim(':') :: None :: tail =>
        buildObj(tail, Json.Obj(curObj.fields + (field -> None)))
      case Str(field) :: Delim(':') :: Delim('{') :: tail =>
        buildObj(tail, Obj(Map())) match {
          case Right((newObj, rest)) =>
            buildObj(rest, Obj(curObj.fields + (field -> newObj)))
          case Left(er) => Left(er)
        }
      case Str(filed) :: Delim(':') :: Delim('[') :: tail =>
        buildArr(tail, Arr(List())) match {
          case Right((newArr, rest)) => buildObj(rest, Obj(curObj.fields + (filed -> newArr)))
          case Left(er) => Left(er)
        }
      case _ => Left("Error lexi parsing")
    }

  def buildArr(tokens: List[Json], curArr: Arr): Either[String, (Json, List[Json])] =
    tokens match {
      case Delim(_) :: Delim('[') :: tail =>
        buildArr(tail, Arr(List())) match {
          case Right((newArr, rest)) => buildArr(rest, Arr(newArr :: curArr.objs))
          case Left(er) => Left(er)
        }
      case Delim('[') :: tail => buildArr(tail, Arr(List()))
      case Delim(']') :: tail => Right((curArr, tail))
      case Delim(',') :: tail => buildArr(tail, curArr)
      case (value: Str) :: tail => buildArr(tail, Arr(value :: curArr.objs))
      case (value: Num) :: tail => buildArr(tail, Arr(value :: curArr.objs))
      case (value: Bool) :: tail => buildArr(tail, Arr(value :: curArr.objs))
      case None :: tail => buildArr(tail, Arr(None :: curArr.objs))
      case Delim('{') :: tail =>
        buildObj(tail, Obj(Map())) match {
          case Right((newObj, rest)) => buildArr(rest, Arr(newObj :: curArr.objs))
          case Left(er) => Left(er)
        }
      case _ => Left("Error lexi parsing")
    }
}
