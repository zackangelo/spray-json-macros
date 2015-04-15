package zangelo.spray.json.annotation

import scala.annotation.StaticAnnotation

case class JsonPropertyCase(value:JsonPropertyCases.JsonPropertyCase) extends StaticAnnotation

object JsonPropertyCases {
  sealed trait JsonPropertyCase
  object Identity extends JsonPropertyCase
  object Snakize extends JsonPropertyCase
  object Capitalize extends JsonPropertyCase
}