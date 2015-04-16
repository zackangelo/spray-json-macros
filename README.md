spray-json-macros
=================

A macro library for generating spray-json JsonFormat instances. *This library is highly experimental and is not
production-ready.* It likely still contains many sharp edges.

* Automatically create JsonFormats for case classes with more than 22 fields
* Support default parameters in static top-level case classes
* Explicitly rename fields using the `@JsonProperty` annotation
* Bring nested case classes into their parent objects using the `@JsonUnwrapped` annotation
* Automatically create JsonFormats for Scala enumerations

### Installation

spray-json-macros is not currently hosted in an artifact repository. To install: clone the repository,
run the `package` sbt task and place the resulting jar in the `/lib' folder of your project.

### Usage

To enable automatic JsonFormat generation for case classes, bring the members of the `AutoProductFormats` into
your implicit scope:

```scala
import spray.json._
import zangelo.spray.json._

object MyJsonProtocol
    extends DefaultJsonProtocol
    with AutoProductFormats

import MyJsonProtocol._

case class Person(firstName:String, lastName:String)

val person = Person("Zack", "Angelo")
val personJson = person.toJson // { "firstName": "Zack", "lastName": "Angelo" }
val personReadBack = personJson.convertTo[Person]
```

#### Annotations

The `@JsonProperty` annotation allows you to explicitly rename properties:

```scala
import spray.json._
import zangelo.spray.json._
import zangelo.spray.json.annotation._

object MyJsonProtocol
    extends DefaultJsonProtocol
    with AutoProductFormats

import MyJsonProtocol._

case class Person(firstName:String, @JsonProperty("surname") lastName:String)

val person = Person("Zack", "Angelo")
val personJson = person.toJson // { "firstName": "Zack", "surname": "Angelo" }
val personReadBack = personJson.convertTo[Person]
```

The `@JsonPropertyCase` annotation allows you to automatically transform the case of your properties:

```scala
import spray.json._
import zangelo.spray.json._
import zangelo.spray.json.annotation._

object MyJsonProtocol
    extends DefaultJsonProtocol
    with AutoProductFormats

import MyJsonProtocol._

@JsonPropertyCase(JsonPropertyCases.Snakize)
case class Person(firstName:String, lastName:String)

val person = Person("Zack", "Angelo")
val personJson = person.toJson // { "first_name": "Zack", "last_name": "Angelo" }
val personReadBack = personJson.convertTo[Person]
```

The `@JsonUnwrapped` annotation allows you bring nested properties their parent's JSON object:

```scala
import spray.json._
import zangelo.spray.json._
import zangelo.spray.json.annotation._

object MyJsonProtocol
    extends DefaultJsonProtocol
    with AutoProductFormats

import MyJsonProtocol._

@JsonPropertyCase(JsonPropertyCases.Capitalize)
case class Address(street:String, city:String, state:String, zip:String)

case class Person(firstName:String,
                  lastName:String,
                  @JsonUnwrapped("workAddress")
                  workAddress:Address,
                  @JsonUnwrapped("homeAddress")
                  homeAddress:Address)

val work = Address("150 Main Street", "New York", "NY", "00000")
val home = Address("600 W. Sixth", "Austin", "TX", "11111")
val person = Person("Zack", "Angelo", work, home)
val personJson = person.toJson

//  {
//      "firstName": "Zack",
//      "lastName": "Angelo",
//      "homeAddressStreet": "600 W. Sixth",
//      "homeAddressCity": "Austin",
//      "homeAddressState": "TX"
//      "homeAddressZip": "11111",
//      "workAddressStreet": "150 Main Street",
//      "workAddressCity": "New York",
//      "workAddressState": "NY",
//      "workAddressZip": "00000"
//  }

val personReadBack = personJson.convertTo[Person]
```
