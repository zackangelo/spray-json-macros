package zangelo.spray.json.annotation

import scala.annotation.StaticAnnotation

final case class JsonProperty(name:String) extends StaticAnnotation
