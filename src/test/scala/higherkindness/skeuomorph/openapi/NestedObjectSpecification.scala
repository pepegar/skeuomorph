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

class NestedObjectSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  import Optimize._
  import cats.syntax.all._
  import higherkindness.droste._
  import helpers._

  def nestedTypesFrom[T: Basis[JsonSchemaF, *]](t: T, map: Map[String, T] = Map.empty[String, T]) = {
    val (x, y) = nestedTypes.apply(t).run(map -> 0).value
    x._1 -> y
  }

  def expectedTypes[T: Basis[JsonSchemaF, *]](d: (String, T)*)(t: T) =
    d.toMap -> t

  "basic types should not change" >> {
    "when integer is provided" >> {
      nestedTypesFrom(Fixed.integer()) must ===(expectedTypes()(Fixed.integer()))
    }
    "when reference is provided" >> {
      nestedTypesFrom(Fixed.reference("Foo")) must ===(expectedTypes()(Fixed.reference("Foo")))
    }
  }

  "complex types should remove nested objects" >> {
    "when an array is provided in the first level" >> {
      val nestedObject = obj("foo" -> Fixed.string())("foo")
      nestedTypesFrom(Fixed.array(nestedObject)) must ===(
        expectedTypes("AnonymousObject" -> nestedObject)(Fixed.array(Fixed.reference("AnonymousObject")))
      )
    }

    "when an object is nested inside another object " >> {
      val nestedObject = obj("name" -> Fixed.string(), "password" -> Fixed.password())("name")
      nestedTypesFrom(obj("user" -> nestedObject, "another" -> Fixed.binary())("user")) must ===(
        expectedTypes("User" -> nestedObject)(
          obj("user" -> Fixed.reference("User"), "another" -> Fixed.binary())("user")
        )
      )
    }

    "when an object is nested inside an array that is nested inside another object" >> {
      val nestedObject = obj("foo" -> Fixed.string())("foo")
      nestedTypesFrom(obj("foo" -> Fixed.array(nestedObject))()) must ===(
        expectedTypes("AnonymousObject" -> nestedObject)(
          obj("foo" -> Fixed.array(Fixed.reference("AnonymousObject")))()
        )
      )
    }

    "when an enum is nested inside an object" >> {
      val nestedEnum = Fixed.enum(List("Blue", "Green"))
      nestedTypesFrom(obj("color" -> nestedEnum)()) must ===(
        expectedTypes(
          "Color" -> nestedEnum
        )(
          obj("color" -> Fixed.reference("Color"))()
        )
      )
    }
    "when an enum is nested inside an array" >> {
      val nestedEnum = Fixed.enum(List("Blue", "Green"))
      nestedTypesFrom(obj("colors" -> Fixed.array(nestedEnum))()) must ===(
        expectedTypes(
          "AnonymousObject" -> nestedEnum
        )(
          obj("colors" -> Fixed.array(Fixed.reference("AnonymousObject")))()
        )
      )
    }

    "when there are multiple level of nested objects" >> {
      nestedTypesFrom(
        obj(
          "foo1" ->
            obj(
              "foo2" ->
                obj(
                  "foo3" ->
                    obj(
                      "foo4" ->
                        obj("foo5" -> Fixed.string())()
                    )()
                )()
            )()
        )()
      ) must ===(
        expectedTypes(
          "Foo4" -> obj("foo5" -> Fixed.string())(),
          "Foo3" -> obj("foo4" -> Fixed.reference("Foo4"))(),
          "Foo2" -> obj("foo3" -> Fixed.reference("Foo3"))(),
          "Foo1" -> obj("foo2" -> Fixed.reference("Foo2"))()
        )(obj("foo1" -> Fixed.reference("Foo1"))())
      )
    }

    "when there are multiple level of nested objects when than alpha characters are the same" >> {
      nestedTypesFrom(
        obj(
          "foo" ->
            obj(
              "foo1" ->
                obj(
                  "foo" ->
                    obj("foo" -> Fixed.string())()
                )()
            )()
        )()
      ) must ===(
        expectedTypes(
          "Foo"  -> obj("foo" -> Fixed.string())(),
          "Foo1" -> obj("foo" -> Fixed.reference("Foo"))(),
          "Foo2" -> obj("foo1" -> Fixed.reference("Foo1"))()
        )(obj("foo" -> Fixed.reference("Foo2"))())
      )
    }

    "when there are multiple level of nested object of different types" >> {
      nestedTypesFrom(
        obj(
          "foo1" ->
            Fixed.array(
              obj(
                "foo2" ->
                  Fixed.array(
                    obj(
                      "foo3" ->
                        Fixed.array(
                          obj(
                            "foo4" ->
                              Fixed.array(obj("foo5" -> Fixed.array(Fixed.string()))())
                          )()
                        )
                    )()
                  )
              )()
            )
        )()
      ) must ===(
        expectedTypes(
          "AnonymousObject3" -> obj("foo2" -> Fixed.array(Fixed.reference("AnonymousObject2")))(),
          "AnonymousObject2" -> obj("foo3" -> Fixed.array(Fixed.reference("AnonymousObject1")))(),
          "AnonymousObject1" -> obj("foo4" -> Fixed.array(Fixed.reference("AnonymousObject")))(),
          "AnonymousObject"  -> obj("foo5" -> Fixed.array(Fixed.string()))()
        )(obj("foo1" -> Fixed.array(Fixed.reference("AnonymousObject3")))())
      )
    }
  }
  "when the name of the anonymous object exists already as a type" >> {
    nestedTypesFrom(
      obj("user" -> obj("name" -> Fixed.string())())(),
      Map("User" -> Fixed.string())
    ) must ===(
      expectedTypes(
        "User"  -> Fixed.string(),
        "User1" -> obj("name" -> Fixed.string())()
      )(obj("user" -> Fixed.reference("User1"))())
    )

  }
}
