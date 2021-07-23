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

class OpenApiNestedObjectSpecification extends org.specs2.mutable.Specification {
  import helpers._
  import schema._
  import JsonSchemaF.Fixed
  import OpenApi._

  "open api should extract nested object" >> {
    "when nested objects are defined in schemas" >> {
      extractNestedTypes(
        openApi("name")
          .withSchema("Foo", obj("bar" -> obj("x" -> Fixed.string())())())
          .withSchema("Foos", Fixed.array(obj("y" -> Fixed.integer())()))
      ) must ===(
        openApi("name")
          .withSchema("Bar", obj("x" -> Fixed.string())())
          .withSchema("Foo", obj("bar" -> Fixed.reference("Bar"))())
          .withSchema("AnonymousObject", obj("y" -> Fixed.integer())())
          .withSchema("Foos", Fixed.array(Fixed.reference("AnonymousObject")))
      )
    }

    "when nested objects are defined in request" >> {
      extractNestedTypes(
        openApi("name").withPath(
          "foo" -> emptyItemObject
            .withPut(
              operation[JsonSchemaF.Fixed](
                request("application/json" -> mediaType(obj("foo" -> Fixed.enum(List("1", "2")))())),
                "200" -> response[JsonSchemaF.Fixed]("Null response")
              )
            )
        )
      ) must ===(
        openApi("name")
          .withPath(
            "foo" -> emptyItemObject
              .withPut(
                operation[JsonSchemaF.Fixed](
                  request("application/json" -> mediaType(obj("foo" -> Fixed.reference("Foo"))())),
                  "200" -> response[JsonSchemaF.Fixed]("Null response")
                )
              )
          )
          .withSchema("Foo", Fixed.enum(List("1", "2")))
      )
    }

    "when nested objects are defined in request" >> {
      extractNestedTypes(
        openApi("name").withPath(
          "foo" -> emptyItemObject
            .withGet(
              operationWithResponses[JsonSchemaF.Fixed](
                "200" -> response[JsonSchemaF.Fixed](
                  "Response",
                  "application/json" -> mediaType(obj("values" -> Fixed.array(obj("value" -> Fixed.integer())()))())
                )
              )
            )
        )
      ) must ===(
        openApi("name")
          .withPath(
            "foo" -> emptyItemObject
              .withGet(
                operationWithResponses[JsonSchemaF.Fixed](
                  "200" -> response[JsonSchemaF.Fixed](
                    "Response",
                    "application/json" -> mediaType(obj("values" -> Fixed.array(Fixed.reference("AnonymousObject")))())
                  )
                )
              )
          )
          .withSchema("AnonymousObject", obj("value" -> Fixed.integer())())
      )
    }
  }
}
