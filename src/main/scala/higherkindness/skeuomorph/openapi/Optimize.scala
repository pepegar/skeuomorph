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
import higherkindness.droste._

import cats.data.State
import cats.syntax.all._
import scala.annotation.tailrec

object Optimize {
  def namedTypesTrans[T](name: String): Trans[JsonSchemaF, JsonSchemaF, T] =
    Trans {
      case JsonSchemaF.ObjectF(_, _) => JsonSchemaF.reference[T](name)
      case JsonSchemaF.EnumF(_)      => JsonSchemaF.reference[T](name)
      case other                     => other
    }

  def namedTypes[T: Basis[JsonSchemaF, *]](name: String): T => T = scheme.cata(namedTypesTrans(name).algebra)

  type NestedTypesState[T, O] = State[(Map[String, T], Long), O]

  def nestedTypesTrans[T: Basis[JsonSchemaF, *]]: TransM[NestedTypesState[T, *], JsonSchemaF, JsonSchemaF, T] =
    TransM {
      case JsonSchemaF.ArrayF(x) if isNestedType(x) =>
        extractNestedTypes("AnonymousObject", x).map { case (n, t) => JsonSchemaF.ArrayF(namedTypes(n).apply(t)) }

      case JsonSchemaF.ObjectF(fields, required) =>
        fields
          .traverse[NestedTypesState[T, *], JsonSchemaF.Property[T]] {
            case p if isNestedType(p.tpe) =>
              extractNestedTypes(p.name.capitalize, p.tpe).map { //TODO Maybe we should normalize
                case (n, t) => p.copy(tpe = namedTypes[T](n).apply(t))
              }
            case p => State.pure(p)
          }
          .map(JsonSchemaF.ObjectF(_, required))

      case other => State.pure(other)
    }

  def nestedTypes[T: Basis[JsonSchemaF, *]]: T => NestedTypesState[T, T] =
    scheme.anaM(nestedTypesTrans.coalgebra)

  private def isNestedType[T: Project[JsonSchemaF, *]](t: T): Boolean = {
    import JsonSchemaF._
    import higherkindness.droste.syntax.project.toProjectSyntaxOps
    t.project match {
      case ObjectF(properties, _) if properties.nonEmpty => true
      case EnumF(_)                                      => true
      case _                                             => false
    }
  }

  private def extractNestedTypes[T: Basis[JsonSchemaF, *]](name: String, tpe: T): NestedTypesState[T, (String, T)] = {
    def addType(items: (String, T)): NestedTypesState[T, Unit] =
      State.modify { case (x, y) =>
        (x + items) -> y
      }

    def generateName(originalName: String, previousOccurrences: Long): String =
      originalName.capitalize + (if (previousOccurrences > 0) previousOccurrences.toString else "")

    @tailrec
    def findNameAndIndex(ix: Long, x: Map[String, _]): (String, Long) = {
      val n = generateName(name, ix)
      if (x.contains(n)) findNameAndIndex(ix + 1, x) else n -> ix
    }

    def findName: NestedTypesState[T, String] =
      State { case (x, i) =>
        val (name, nextI) = findNameAndIndex(i, x)
        (x, nextI) -> name
      }

    for {
      newType <- nestedTypes.apply(tpe)
      newName <- findName
      _       <- addType(newName -> newType)
    } yield newName -> newType
  }

}
