import zangelo.spray.json.AutoProductFormats
import zangelo.spray.json.annotation._
import spray.json._

@JsonPropertyCase(JsonPropertyCases.Snakize)
case class NestedTest(nestedA:String, nestedB:Int)

@JsonPropertyCase(JsonPropertyCases.Snakize)
case class Test2(@JsonProperty("HAI") aahBee:Int,
                 beeCee:String,
                 nestedProperty:NestedTest)

case class Box[T](v:T)

object JsonProtocol
  extends DefaultJsonProtocol
  with AutoProductFormats

import JsonProtocol._

//val test2json = Test2(1,"vvv",NestedTest("nested value", 500)).toJson
//test2json.convertTo[Test2]

val boxy = Box("hullo")

val boxyJson = boxy.toJson

val reboxy = boxyJson.convertTo[Box[String]]

