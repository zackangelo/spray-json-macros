package zangelo.spray.json.annotation

import scala.annotation.StaticAnnotation

case class JsonUnwrapped(prefix:String = "", suffix:String = "") extends StaticAnnotation
