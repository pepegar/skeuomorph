/*
 * Copyright 2018-2021 47 Degrees Open Source <https://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package higherkindness.skeuomorph.openapi

import io.circe.Decoder
import JsonDecoders._
import io.circe.Json
import cats.syntax.all._
import helpers._

class JsonSchemaDecoderSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  val decoder = Decoder[JsonSchemaF.Fixed]

  "Decoder[JsonSchemaF.Fixed] should able to decode" >> {
    "when integer object is provided without format" >> {
      decoder.decodeJson(basicTypeFrom("integer")) must beRight(Fixed.integer())
    }
    "when integer object is provided" >> {
      decoder.decodeJson(basicTypeFrom("integer", format = "int32".some)) must beRight(Fixed.integer())
    }
    "when long object is provided" >> {
      decoder.decodeJson(basicTypeFrom("integer", format = "int64".some)) must beRight(Fixed.long())
    }
    "when number object is provided without format" >> {
      decoder.decodeJson(basicTypeFrom("number")) must beRight(Fixed.float())
    }
    "when meters object is provided" >> {
      decoder.decodeJson(basicTypeFrom("integer", format = "meters".some)) must beRight(Fixed.integer())
    }
    "when float object is provided" >> {
      decoder.decodeJson(basicTypeFrom("number", format = "float".some)) must beRight(Fixed.float())
    }
    "when double object is provided" >> {
      decoder.decodeJson(basicTypeFrom("number", format = "double".some)) must beRight(Fixed.double())
    }
    "when number object is provided without format" >> {
      decoder.decodeJson(basicTypeFrom("number", format = "dollar".some)) must beRight(Fixed.float())
    }
    "when string object is provided" >> {
      decoder.decodeJson(basicTypeFrom("string")) must beRight(Fixed.string())
    }
    "when byte object is provided" >> {
      decoder.decodeJson(basicTypeFrom("string", format = "byte".some)) must beRight(Fixed.byte())
    }
    "when email object is provided" >> {
      decoder.decodeJson(basicTypeFrom("string", format = "email".some)) must beRight(Fixed.string())
    }

    "when binary object is provided" >> {
      decoder.decodeJson(basicTypeFrom("string", format = "binary".some)) must beRight(Fixed.binary())
    }
    "when boolean object is provided" >> {
      decoder.decodeJson(basicTypeFrom("boolean")) must beRight(Fixed.boolean())
    }
    "when date object is provided" >> {
      decoder.decodeJson(basicTypeFrom("string", format = "date".some)) must beRight(Fixed.date())
    }
    "when datetime object is provided" >> {
      decoder.decodeJson(basicTypeFrom("string", format = "date-time".some)) must beRight(Fixed.dateTime())
    }
    "when password object is provided" >> {
      decoder.decodeJson(basicTypeFrom("string", format = "password".some)) must beRight(Fixed.password())
    }

    "when reference object is provided" >> {
      decoder.decodeJson(Json.obj(s"$$ref" -> Json.fromString("/my/ref"))) must beRight(Fixed.reference("/my/ref"))
    }
    "when enum object is provided" >> {
      val enumType = Json.obj(
        "type" -> Json.fromString("string"),
        "enum" -> Json.arr(
          Json.fromString("green"),
          Json.fromString("blue")
        )
      )
      decoder.decodeJson(enumType) must beRight(Fixed.enum(List("green", "blue")))
    }
    "when sum object is provided" >> {
      val sumType = Json.obj(
        "oneOf" -> Json.arr(
          Json.obj(s"$$ref" -> Json.fromString("#/components/schemas/Cat")),
          Json.obj(s"$$ref" -> Json.fromString("#/components/schemas/Dog"))
        )
      )
      decoder.decodeJson(sumType) must beRight(
        Fixed.sum(List(Fixed.reference("#/components/schemas/Cat"), Fixed.reference("#/components/schemas/Dog")))
      )
    }
    "when array object is provided" >> {
      val arrayType = Json.obj(
        "type" -> Json.fromString("array"),
        "items" -> Json.obj(
          "type" -> Json.fromString("integer")
        )
      )
      decoder.decodeJson(arrayType) must beRight(Fixed.array(Fixed.integer()))
    }

    "when object object is provided" >> {
      val objectType = Json.obj(
        "type" -> Json.fromString("object"),
        "properties" -> Json.obj(
          "name" -> Json.obj("type" -> Json.fromString("string")),
          "age" -> Json.obj(
            "type"    -> Json.fromString("integer"),
            "format"  -> Json.fromString("int32"),
            "minimum" -> Json.fromInt(0)
          ),
          "address" -> Json.obj(s"$$ref" -> Json.fromString("#/components/schemas/Address"))
        ),
        "required" -> Json.arr(Json.fromString("name"))
      )
      decoder.decodeJson(objectType) must beRight(
        obj(
          "name"    -> Fixed.string(),
          "age"     -> Fixed.integer(),
          "address" -> Fixed.reference("#/components/schemas/Address")
        )("name")
      )
    }

    "when object object is provided without required field" >> {
      val objectType = Json.obj(
        "type" -> Json.fromString("object"),
        "properties" -> Json.obj(
          "valid" -> Json.obj("type" -> Json.fromString("boolean"))
        )
      )
      decoder.decodeJson(objectType) must beRight(obj("valid" -> Fixed.boolean())())
    }

    "when object does not have properties" >> {
      decoder
        .decodeJson(
          Json.obj(
            "type"                 -> Json.fromString("object"),
            "additionalProperties" -> Json.obj("type" -> Json.fromString("object"))
          )
        ) must beRight(obj()())
    }

    "when object does have type" >> {
      decoder.decodeJson(
        Json.obj(
          "description" -> Json.fromString("subscription information"),
          "properties" -> Json.obj(
            "subscriptionId" -> Json.obj(
              "description" -> Json.fromString("this unique identifier allows management of the subscription"),
              "type"        -> Json.fromString("string"),
              "example"     -> Json.fromString("2531329f-fb09-4ef7-887e-84e648214436")
            )
          ),
          "required" -> Json.arr(Json.fromString("subscriptionId"))
        )
      ) must beRight(obj("subscriptionId" -> Fixed.string())("subscriptionId"))
    }

  }

  "Decoder[JsonSchemaF.Fixed] should not able to decode" >> {
    "when the type is not valid" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("foo"))).leftMap(_.message) must beLeft(
        "foo is not well formed type"
      )
    }
    "when it is not valid schema" >> {
      decoder.decodeJson(Json.fromString("string")).leftMap(_.message) must beLeft(
        "Attempt to decode value on failed cursor"
      )
    }

    "when array does not have values" >> {
      decoder.decodeJson(Json.obj("type" -> Json.fromString("array"))).leftMap(_.message) must beLeft(
        "array is not well formed type"
      )
    }

  }

  def basicTypeFrom(tpe: String, format: Option[String] = None): Json =
    format.fold(
      Json
        .obj(
          "type" -> Json.fromString(tpe)
        )
    )(x => Json.obj("type" -> Json.fromString(tpe), "format" -> Json.fromString(x)))

}
