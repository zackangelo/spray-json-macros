package zangelo.spray.json

import org.specs2.mutable.Specification
import spray.json._
import zangelo.spray.json.annotation._


trait TestObject extends Product

object TestProtocol
  extends DefaultJsonProtocol
  with AutoProductFormats[TestObject]

//TODO needs to be a top-level class so companion symbol is accessible
case class TestDefault(a:String = "a", b:Int) extends TestObject

/**
 * Mostly adapted from spray-json's ProductFormatsSpec
 */
class AutoProductFormatsSpec extends Specification {

  case class Test0() extends TestObject

  case class Test2(a:Int, b:Option[Double]) extends TestObject

  case class Test3[A, B](as: List[A], bs: List[B]) extends TestObject

  case class Test36(a1: String,
                    a2: String,
                    a3: String,
                    a4: String,
                    a5: Int,
                    a6: String,
                    a7: String,
                    a8: String,
                    a9: String,
                    a10: String,
                    a11: String,
                    a12: Double,
                    a13: String,
                    a14: String,
                    a15: String,
                    a16: String,
                    a17: String,
                    a18: String,
                    a19: String,
                    a20: String,
                    a21: String,
                    a22: String,
                    a23: Int,
                    a24: String,
                    a25: String,
                    a26: String,
                    a27: String,
                    a28: String,
                    a29: String,
                    a30: Double,
                    a31: String,
                    a32: String,
                    a33: String,
                    a34: String,
                    a35: String,
                    a36: String) extends TestObject

  @SerialVersionUID(1L) // SerialVersionUID adds a static field to the case class
  case class TestStatic(a: Int, b: Option[Double]) extends TestObject
  case class TestMangled(`foo-bar!`: Int) extends TestObject

  "A JsonFormat created with `autoProductFormat`, for a case class with 2 elements," should {
    import TestProtocol._

    val json = JsObject("a" -> JsNumber(42), "b" -> JsNumber(4.2))
    val obj = Test2(42, Some(4.2))

    val genJson = obj.toJson

    "convert to a respective JsObject" in {
      genJson mustEqual json
    }

    "convert a JsObject to the respective case class instance" in {
      json.convertTo[Test2] mustEqual obj
    }

    "throw a DeserializationException if the JsObject does not all required members" in (
      JsObject("b" -> JsNumber(4.2)).convertTo[Test2] must
        throwA(new DeserializationException("Object is missing required member 'a'"))
      )

    "not require the presence of optional fields for deserialization" in {
      JsObject("a" -> JsNumber(42)).convertTo[Test2] mustEqual Test2(42, None)
    }

    "not render `None` members during serialization" in {
      Test2(42, None).toJson mustEqual JsObject("a" -> JsNumber(42))
    }

    "ignore additional members during deserialization" in {
      JsObject("a" -> JsNumber(42), "b" -> JsNumber(4.2), "c" -> JsString('no)).convertTo[Test2] mustEqual obj
    }

    "not depend on any specific member order for deserialization" in {
      JsObject("b" -> JsNumber(4.2), "a" -> JsNumber(42)).convertTo[Test2] mustEqual obj
    }

//FIXME throw correct exception
//    "throw a DeserializationException if the JsValue is not a JsObject" in {
//      JsNull.convertTo[Test2] must throwA(new DeserializationException("Object expected in field 'a'"))
//    }
  }

  "A JsonFormat for a generic case class and created with `autoProductFormat`" should {
    import TestProtocol._

    val obj = Test3(42 :: 43 :: Nil, "x" :: "y" :: "z" :: Nil)

    val json = JsObject(
      "as" -> JsArray(JsNumber(42), JsNumber(43)),
      "bs" -> JsArray(JsString("x"), JsString("y"), JsString("z"))
    )
    "convert to a respective JsObject" in {
      obj.toJson mustEqual json
    }
    "convert a JsObject to the respective case class instance" in {
      json.convertTo[Test3[Int, String]] mustEqual obj
    }
  }

  "A JsonFormat for a case class with 36 parameters and created with `autoProductFormat`" should {
    import TestProtocol._

    val obj = Test36(
      "a1", "a2", "a3", "a4", 5, "a6", "a7", "a8", "a9",
      "a10", "a11", 12d, "a13", "a14", "a15", "a16", "a17", "a18",
      "a1", "a2", "a3", "a4", 5, "a6", "a7", "a8", "a9",
      "a10", "a11", 12d, "a13", "a14", "a15", "a16", "a17", "a18")

    val json = JsonParser("""{"a28":"a10","a17":"a17","a34":"a16","a6":"a6","a30":12.0,"a24":"a6","a13":"a13","a29":"a11","a35":"a17","a18":"a18","a5":5,"a4":"a4","a9":"a9","a25":"a7","a14":"a14","a15":"a15","a26":"a8","a36":"a18","a11":"a11","a22":"a4","a33":"a15","a10":"a10","a3":"a3","a21":"a3","a8":"a8","a32":"a14","a1":"a1","a16":"a16","a27":"a9","a20":"a2","a7":"a7","a12":12.0,"a23":5,"a2":"a2","a19":"a1","a31":"a13"}""")

    "convert to a respective JsObject" in {
      obj.toJson mustEqual json
    }
    "convert a JsObject to the respective case class instance" in {
      json.convertTo[Test36] mustEqual obj
    }
  }

  "A JsonFormat for a generic case class with an explicitly provided type parameter" should {
    "serialize to the correct type parameter" in {
      import TestProtocol._

      case class Box[A](a: A) extends TestObject
      Box(42).toJson === JsObject(Map("a" -> JsNumber(42)))
    }
  }

  //TODO support transient fields
//  "A JsonFormat for a case class with transient fields and created with `jsonFormat`" should {
//    import TestProtocol1._
//    val obj = TestTransient(42, Some(4.2))
//    val json = JsObject("a" -> JsNumber(42), "b" -> JsNumber(4.2))
//    "convert to a respective JsObject" in {
//      obj.toJson mustEqual json
//    }
//    "convert a JsObject to the respective case class instance" in {
//      json.convertTo[TestTransient] mustEqual obj
//    }
//  }
//
    "A JsonFormat for a case class with static fields and created with `autoProductFormat`" should {
      import TestProtocol._
      val obj = TestStatic(42, Some(4.2))
      val json = JsObject("a" -> JsNumber(42), "b" -> JsNumber(4.2))
      "convert to a respective JsObject" in {
        obj.toJson mustEqual json
      }
      "convert a JsObject to the respective case class instance" in {
        json.convertTo[TestStatic] mustEqual obj
      }
    }

  "A JsonFormat created with `autoProductFormat`, for a case class with 0 elements," should {
    import TestProtocol._

    val obj = Test0()
    val json = JsObject()
    "convert to a respective JsObject" in {
      obj.toJson mustEqual json
    }

    "convert a JsObject to the respective case class instance" in {
      json.convertTo[Test0] mustEqual obj
    }

    "ignore additional members during deserialization" in {
      JsObject("a" -> JsNumber(42)).convertTo[Test0] mustEqual obj
    }

// FIXME not sure why this is failing
//    "throw a DeserializationException if the JsValue is not a JsObject" in (
//      JsNull.convertTo[Test0] must throwA(new DeserializationException("JSON object expected instead"))
//      )
  }

  "A JsonFormat created with `autoProductFormat`, for a case class with mangled-name members," should {
    import TestProtocol._
    val json = "{\"foo-bar!\":42}"
    "produce the correct JSON" in {
      TestMangled(42).toJson.compactPrint === json
    }
    "convert a JsObject to the respective case class instance" in {
      json.parseJson.convertTo[TestMangled] === TestMangled(42)
    }
  }

  "A JsonFormat created with `autoProductFormat`, for a case class with @JsonProperty annotated members," should {
    import TestProtocol._
    case class TestAnnotated(@JsonProperty("overridden") a:String, b:Int) extends TestObject
    val obj = TestAnnotated("a", 42)
    val json = JsObject("overridden" -> JsString("a"), "b" -> JsNumber(42))

    "rename the JSON property according to the annotation's value" in {
      obj.toJson shouldEqual json
    }

    "convert a JsObject to the respective case class instance" in {
      json.convertTo[TestAnnotated] shouldEqual obj
    }
  }

  "A JsonFormat created with `autoProductFormat`, for a case class with default members," should {
    import TestProtocol._

    val obj = TestDefault(b = 42)

    val json = JsObject("a" -> JsString("a"), "b" -> JsNumber(42))
    val jsonWithDefaultMissing = JsObject("b" -> JsNumber(42))

    "convert to a respective JsObject with default parameter as a field" in {
      obj.toJson shouldEqual json
    }

    "convert a JsObject with the default parameter missing to the respective case class instance" in {
      jsonWithDefaultMissing.convertTo[TestDefault] shouldEqual obj
    }
  }

  "A JsonFormat created with `autoProductFormat`, for a case class with @JsonUnwrapped annotated members," should {
    import TestProtocol._

    case class Nested(c:String, d:Option[Double]) extends TestObject
    case class TestAnnotated(a:String, @JsonUnwrapped b:Nested) extends TestObject
    case class TestAnnotatedPrefix(a:String, @JsonUnwrapped("pre_") b:Nested) extends TestObject
    case class TestAnnotatedSuffix(a:String, @JsonUnwrapped("","_suf") b:Nested) extends TestObject

    val obj = TestAnnotated("a", Nested("c", Some(42.0)))
    val objPrefix = TestAnnotatedPrefix("a", Nested("c", Some(42.0)))
    val objSuffix = TestAnnotatedSuffix("a", Nested("c", Some(42.0)))

    val json = JsObject("a" -> JsString("a"), "c" -> JsString("c"), "d" -> JsNumber(42.0))
    val prefixJson = JsObject("a" -> JsString("a"), "pre_c" -> JsString("c"), "pre_d" -> JsNumber(42.0))
    val suffixJson = JsObject("a" -> JsString("a"), "c_suf" -> JsString("c"), "d_suf" -> JsNumber(42.0))

    "bring the nested object's properties into the top-level JSON" in {
      obj.toJson shouldEqual json
    }

    "convert a JsObject to the respective case class instance" in {
      json.convertTo[TestAnnotated] shouldEqual obj
    }

    "should add a prefix to the nested keys when converted to a JsObject" in {
      objPrefix.toJson shouldEqual prefixJson
    }

    "convert a JsObject with prefixed keys to the respective case class instance" in {
      prefixJson.convertTo[TestAnnotatedPrefix] shouldEqual objPrefix
    }

    "should add a suffix to the nested keys when converted to a JsObject" in {
      objSuffix.toJson shouldEqual suffixJson
    }

    "convert a JsObject with suffixed keys to the respective case class instance" in {
      suffixJson.convertTo[TestAnnotatedSuffix] shouldEqual objSuffix
    }

  }

  "A JsonFormat created with `autoProductFormat`, for a case class with @JsonPropertyCase annotations," should {
    import TestProtocol._

    case class TestSingleArg(@JsonPropertyCase(JsonPropertyCases.Snakize) twoWordsA:String,
                             twoWordsB:String)
      extends TestObject

    @JsonPropertyCase(JsonPropertyCases.Snakize)
    case class TestAllArgs(twoWordsA:String, twoWordsB:String) extends TestObject

    val singleArgObject = TestSingleArg("a", "b")
    val allArgsObject = TestAllArgs("a", "b")

    val singleArgJson = JsObject("two_words_a" -> JsString("a"), "twoWordsB" -> JsString("b"))
    val allArgsJson = JsObject("two_words_a" -> JsString("a"), "two_words_b" -> JsString("b"))

    "convert to a respective JsObject with the case of a single argument converted to snake case" in {
      singleArgObject.toJson shouldEqual singleArgJson
    }

    "convert a JsObject with a single snakized key to a respective case class instance" in {
      singleArgJson.convertTo[TestSingleArg] shouldEqual singleArgObject
    }

    "convert to a respective JsObject with the case of a all arguments converted to snake case" in {
      allArgsObject.toJson shouldEqual allArgsJson
    }

    "convert a JsObject with all snakized keys to a respective case class instance" in {
      allArgsJson.convertTo[TestAllArgs] shouldEqual allArgsObject
    }

  }

  "A JsonFormat created with `autoProductFormat`, for a case class with @JsonIgnore annotations," should {

    import TestProtocol._
    
    case class TestIgnore(@JsonIgnore a:String, b:Int,
                          @JsonIgnore c:Int, d:String)
      extends TestObject


    val ignoreObject = TestIgnore("a", 42, 5, "d")
    val ignoreJson   = JsObject("b" -> JsNumber(42), "d" -> JsString("d"))

    "ignore properties with the @JsonIgnore annotation" in {
      ignoreObject.toJson shouldEqual ignoreJson
    }
  }
}