/*
 * Copyright 2018-2019 47 Degrees, LLC. <http://www.47deg.com>
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

package higherkindness.skeuomorph.openapi.client

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer._
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi.JsonSchemaF
import cats.implicits._

import qq.droste._

object print {
  import higherkindness.skeuomorph.openapi.print.{componentsRegex, schema}
  import higherkindness.skeuomorph.openapi.schema._
  import higherkindness.skeuomorph.openapi.schema.Path._

  case class Tpe[T](tpe: Either[String, T], required: Boolean)
  object Tpe {
    def unit[T]: Tpe[T]                                    = Tpe("Unit".asLeft, true)
    def apply[T](name: String): Tpe[T]                     = Tpe(name.asLeft, true)
    def apply[T](tpe: T, required: Boolean = true): Tpe[T] = Tpe(tpe.asRight, required)

    def name[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): String = tpe.tpe.fold(
      identity,
      x => schema(none).print(x).capitalize
    )
    def option[T: Basis[JsonSchemaF, ?]](tpe: Tpe[T]): Either[String, String] =
      if (tpe.required)
        name(tpe).asRight
      else
        name(tpe).asLeft

  }
  case class Var[T](name: Option[String], tpe: Tpe[T])
  object Var {
    def tpe[T](tpe: Tpe[T]): Var[T]                               = Var(None, tpe)
    def apply[T](name: String, tpe: T, required: Boolean): Var[T] = Var(name.some, Tpe(tpe.asRight, required))
  }

  def tpe[T: Basis[JsonSchemaF, ?]]: Printer[Tpe[T]] =
    ((konst("Option[") *< string >* konst("]")) >|< string)
      .contramap(Tpe.option[T])

  def parameter[T: Basis[JsonSchemaF, ?]]: Printer[Var[T]] =
    (
      string >* konst(": "),
      tpe
    ).contramapN(x => decapitalize(x.name.getOrElse(Tpe.name(x.tpe))) -> x.tpe) //FIXME .get

  def requestTuple[T: Basis[JsonSchemaF, ?]](request: Request[T]): Option[Var[T]] =
    request.content
      .get("application/json")
      .flatMap(x => x.schema.map(x => Var.tpe[T](Tpe(x, request.required))))

  def referenceTuple[T: Basis[JsonSchemaF, ?]](reference: Reference): Option[Var[T]] = reference.ref match {
    case componentsRegex(name) => Var.tpe(Tpe.apply(name)).some
    case _                     => none
  }

  def parameterTuple[T: Basis[JsonSchemaF, ?]](parameter: Either[Parameter[T], Reference]): Option[Var[T]] =
    parameter.fold(
      x => Var(x.name, x.schema, x.required).some,
      referenceTuple[T]
    )

  def opTuple[T: Basis[JsonSchemaF, ?]](
      operation: OperationWithPath[T]): (String, (List[Var[T]]), ResponsesWithOperationId[T]) = {
    val operationId = operation._3.operationId.getOrElse(???) //FIXME What's happen when there is not operationId?
    (
      operationId,
      operation._3.parameters.flatMap(parameterTuple[T]) ++
        operation._3.requestBody.flatMap(_.fold[Option[Var[T]]](requestTuple, referenceTuple)),
      operationId -> operation._3.responses)
  }

  type OperationWithPath[T]        = (String, String, Operation[T])
  type ResponsesWithOperationId[T] = (String, Map[String, Either[Response[T], Reference]])
  type TraitName                   = String

  def responseType[T: Basis[JsonSchemaF, ?]](name: String): Printer[Response[T]] =
    tpe.contramap(x =>
      x.content.get("application/json").map(_.schema.fold(Tpe[T](name))(Tpe(_, true))).getOrElse(Tpe.unit[T]))

  def responseOrType[T: Basis[JsonSchemaF, ?]](operationId: String): Printer[Either[Response[T], Reference]] =
    responseType(operationId) >|< tpe.contramap(x => referenceTuple(x).map(_.tpe).getOrElse(Tpe.unit))

  def responsesTypes[T: Basis[JsonSchemaF, ?]]: Printer[ResponsesWithOperationId[T]] = Printer {
    case (x, y) =>
      val defaultName = s"${x.capitalize}Response"
      y.size match {
        case 0 => "Unit"
        case 1 => responseOrType(defaultName).print(y.head._2)
        case _ => defaultName
      }
  }

  def method[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] =
    (
      konst("  def ") *< string,
      konst("(") *< sepBy(parameter, ", "),
      konst("): F[") *< responsesTypes >* konst("]")
    ).contramapN(opTuple[T])

  def itemObjectTuple[T: Basis[JsonSchemaF, ?]](xs: (String, ItemObject[T])): List[OperationWithPath[T]] = {
    val (path, itemObject) = xs
    List(
      itemObject.get.map(("get", path, _)),
      itemObject.delete.map(("delete", path, _)),
      itemObject.post.map(("post", path, _)),
      itemObject.put.map(("put", path, _))
    ).flatten
  }

  def responseSchema[T: Basis[JsonSchemaF, ?]]: Printer[OperationWithPath[T]] = Printer(_ => "")

  def clientTypes[T: Basis[JsonSchemaF, ?]]: Printer[(TraitName, List[OperationWithPath[T]])] =
    (
      konst("object ") *< string >* konst("Client {") >* newLine,
      sepBy(responseSchema[T], "") >* (newLine >* konst("}")))
      .contramapN(identity)

  def operationsTuple[T: Basis[JsonSchemaF, ?]](x: (TraitName, Map[String, ItemObject[T]])): (
      TraitName,
      TraitName,
      List[OperationWithPath[T]],
      (TraitName, List[OperationWithPath[T]])) =
    un(second(duplicate(second(x)(_.toList.flatMap(itemObjectTuple[T])))) { case (a, b) => (a, (x._1, b)) })

  def operations[T: Basis[JsonSchemaF, ?]]: Printer[(TraitName, Map[String, ItemObject[T]])] =
    (
      konst("trait ") *< string >* konst("Client[F[_]] {") >* newLine,
      (space >* space >* konst("import ")) *< string >* konst("Client._") >* newLine,
      sepBy(method[T], "\n") >* (newLine >* konst("}") >* newLine),
      clientTypes[T]
    ).contramapN(operationsTuple[T])

  def un[A, B, C, D](pair: ((A, B), (C, D))): (A, B, C, D) = (pair._1._1, pair._1._2, pair._2._1, pair._2._2)
  def duplicate[A, B](pair: (A, B)): ((A, A), (B, B))      = (pair._1 -> pair._1, pair._2 -> pair._2)
  def second[A, B, C](pair: (A, B))(f: B => C): (A, C)     = (pair._1, f(pair._2))
  def decapitalize(s: String)                              = if (s.isEmpty) s else s(0).toLower + s.substring(1)

}