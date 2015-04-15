package zangelo.spray.json

import zangelo.spray.json.annotation._
import spray.json._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

trait AutoProductFormats {
  implicit def implicitAutoProductFormat[T <: Product]:JsonFormat[T] = macro AutoProductFormatMacros.jsonFormatMacro[T]
}

object AutoProductFormats extends AutoProductFormats

object AutoProductFormatMacros {
  private def camelize(word: String): String = {
    val w = pascalize(word)
    w.substring(0, 1).toLowerCase(java.util.Locale.ENGLISH) + w.substring(1)
  }

  private def pascalize(word: String): String = {
    val lst = word.split("_").toList
    (lst.headOption.map(s ⇒ s.substring(0, 1).toUpperCase(java.util.Locale.ENGLISH) + s.substring(1)).get ::
      lst.tail.map(s ⇒ s.substring(0, 1).toUpperCase + s.substring(1))).mkString("")
  }

  private def snakize(word:String) = {
    val spacesPattern = "[-\\s]".r
    val firstPattern = "([A-Z]+)([A-Z][a-z])".r
    val secondPattern = "([a-z\\d])([A-Z])".r
    val replacementPattern = "$1_$2"

    spacesPattern.replaceAllIn(
      secondPattern.replaceAllIn(
        firstPattern.replaceAllIn(
          word, replacementPattern), replacementPattern), "_").toLowerCase
  }

  def jsonFormatMacro[T <: Product : c.WeakTypeTag](c: blackbox.Context):c.Expr[JsonFormat[T]] = {

    import c.universe._

    val ttag = weakTypeOf[T]

    val caseAccessors = ttag.members.collect {
      case m: MethodSymbol if m.isCaseAccessor =>
        m.name.toString -> Select(Ident(TermName("obj")),m.name)
    }

    val ctor = ttag.decls.find(_.isConstructor).get.asMethod
    val ctorParams = ctor.paramLists.head
    val ctorParamsByName = ctorParams.map(p => p.name.toString -> p).toMap

    def jsoCtorArgs = caseAccessors.map { acc =>
      val jsonPropertyName = jsonPropertyForParamName(acc._1)
      q"$jsonPropertyName -> ${acc._2}.toJson"
    }

    def jsonPropertyAnnotation(paramName:String):Option[JsonProperty] = {
      ctorParamsByName(paramName).annotations.map(_.tree).collectFirst {
        case x @ Apply(_, List(Literal(Constant(jsonProperty: String))))
          if x.tpe =:= c.weakTypeOf[JsonProperty] =>
          JsonProperty(jsonProperty)
      }
    }

    def jsonPropertyCaseAnnotation(paramName:String):Option[JsonPropertyCase] = {
      (ctorParamsByName(paramName).annotations ++ ttag.typeSymbol.annotations).map(_.tree).collectFirst {
        case x @ Apply(_, List(Select(_, propertyCaseTerm))) if x.tpe =:= c.weakTypeOf[JsonPropertyCase] =>
          val propertyCase = propertyCaseTerm match {
            case TermName("Snakize")    => JsonPropertyCases.Snakize
            case TermName("Capitalize") => JsonPropertyCases.Capitalize
            case TermName("Identity")   => JsonPropertyCases.Identity
            case unknown =>
              c.warning(c.enclosingPosition, s"Unknown JsonPropertyCase value: $unknown")
              JsonPropertyCases.Identity
          }

          JsonPropertyCase(propertyCase)
      }
    }

    def jsonUnwrappedAnnotation(paramName:String):Option[JsonUnwrapped] = {
      def parseUnwrappedParam(paramTree:Tree):String = paramTree match {
        case Literal(Constant(c:String)) => c
        case Select(_, TermName(termName)) if termName.startsWith("$lessinit$greater$default") => ""
      }

      ctorParamsByName(paramName).annotations.map(_.tree).collectFirst {
        case x @ Apply(_, List(prefixParam, suffixParam)) if x.tpe =:= c.weakTypeOf[JsonUnwrapped] =>
          JsonUnwrapped(parseUnwrappedParam(prefixParam), parseUnwrappedParam(suffixParam))
      }
    }

    def jsonPropertyForParamName(paramName:String) = {
      val propertyExplicitName = jsonPropertyAnnotation(paramName).map(_.name)

      val casedName = jsonPropertyCaseAnnotation(paramName).map(_.value).map {
        case x if x == JsonPropertyCases.Capitalize => pascalize(paramName)
        case x if x == JsonPropertyCases.Snakize    => snakize(paramName)
        case x if x == JsonPropertyCases.Identity   => paramName
      }

      val identity = Some(paramName)

      Seq(propertyExplicitName, casedName, identity).flatten.head
    }

    def writerDefs = ctorParams.map { p =>
      val paramName = p.name.toString

      val jsonDefParams = List(List(ValDef(Modifiers(), TermName("obj"), TypeTree(ttag.finalResultType), EmptyTree)))

      DefDef(Modifiers(Flag.PROTECTED),
        TermName(s"write_$paramName"),
        List.empty,
        jsonDefParams,
        TypeTree(typeOf[List[JsField]]),
        writerDefBody(p.asTerm)
      )
    }

    def writerDefBody(paramSymbol:TermSymbol) = {
      val paramName = paramSymbol.name.toString
      val jsonPropertyName = jsonPropertyForParamName(paramName)
      val toJson = Select(Select(Ident(TermName("obj")), TermName(paramName)), TermName("toJson"))

      jsonUnwrappedAnnotation(paramName) match {
        case Some(unwrapped) =>
          //TODO apply prefix/suffix at compile time?
          q"""$toJson.asJsObject.fields.map { e:(String,JsValue) =>
                (${unwrapped.prefix} + e._1 + ${unwrapped.suffix}) -> e._2
          }.toList"""
        case None =>
          q"""List($jsonPropertyName -> $toJson)"""
      }
    }

    def writerApplyArgs = {
      ctorParams.map { p =>
        val pn      = p.name.toString
        val pnNamed = TermName(pn)
        val pnFn    = TermName(s"write_$pn")

        q"$pnFn(obj)"
      }
    }

    def readerDefs = ctorParams.zipWithIndex.map { i =>
      val (p,index) = i
      val paramName = p.name.toString
      val paramType = p.typeSignature.asSeenFrom(ttag.finalResultType, ttag.typeSymbol.asClass)
      val jsonDefParams = List(List(ValDef(Modifiers(), TermName("jsonFields"), TypeTree(typeOf[JsObject]), EmptyTree)))
      val jsonPropertyName = jsonPropertyForParamName(paramName)

      DefDef(Modifiers(Flag.PROTECTED),
        TermName(s"read_$paramName"),
        List.empty,
        jsonDefParams,
        TypeTree(paramType),
        readerDefBody(index,p.asTerm,paramName,paramType,jsonPropertyName)
      )
    }

    def readerDefBody(index:Int, paramSymbol:TermSymbol, fieldName:String, fieldType:Type, jsonPropertyName:String) = {
      val paramName = paramSymbol.name.toString

      jsonUnwrappedAnnotation(paramName) match {
        case Some(unwrapped) =>
          val pfx = unwrapped.prefix
          val sfx = unwrapped.suffix
          q"""val stripped = jsonFields.fields.collect {
                case (k,v) if k.startsWith($pfx) && k.endsWith($sfx) =>
                  k.stripPrefix($pfx).stripSuffix($sfx) -> v
              }

              JsObject(stripped).convertTo[$fieldType]
           """
        case None =>
          if(paramSymbol.isParamWithDefault) {
            val companion = ttag.typeSymbol.companion
            val defaultMethod = TermName("apply$default$" + (index + 1))
            q"""jsonFields.fields.get($jsonPropertyName).map { v =>
              v.convertTo[$fieldType]
            }.getOrElse($companion.$defaultMethod)
           """
          } else {
            q"""jsonFields.fields($jsonPropertyName).convertTo[$fieldType]"""
          }
      }
    }

    def ctorArgs = ctorParams.map { p =>
      val pn      = p.name.toString
      val pnNamed = TermName(pn)
      val pnFn    = TermName(s"read_$pn")
      q"$pnNamed = $pnFn(jsonFields)"
    }

    val f = c.Expr[JsonFormat[T]](
    q"""new JsonFormat[$ttag] {
         ..$writerDefs

         ..$readerDefs

         override def write(obj: $ttag): JsValue = {
            val fields = List(..$writerApplyArgs).flatten.toMap[String,JsValue]
            JsObject(fields)
          }

         override def read(obj: JsValue):$ttag = {
           val jsonFields = obj.asJsObject
           new $ttag(..$ctorArgs)
         }
       }
    """)

    println(f)

    f
  }
}