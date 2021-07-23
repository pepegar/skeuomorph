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

package higherkindness.skeuomorph.openapi.client

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer.{konst => κ, _}
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi
import higherkindness.skeuomorph.openapi.{schema => _, _}
import higherkindness.skeuomorph.openapi.print._
import cats.syntax.all._
import cats.Show

import higherkindness.droste._

object print {
  import higherkindness.skeuomorph.openapi.schema._
  import higherkindness.skeuomorph.openapi.schema.Path._

  private val jsonMediaType = "application/json"
  private val allContent    = "*/*"

  private val parametersPackage = "parameters"

  def jsonFrom[T](content: Map[String, MediaType[T]]): Option[MediaType[T]] =
    content.get(jsonMediaType) orElse content.get(allContent)

  val statusCodePattern = """(\d+)""".r
  def successStatusCode(code: String): Boolean =
    code match {
      case statusCodePattern(code) => code.toInt >= 200 && code.toInt < 400
      case _                       => false
    }

  def componentsFrom[T](openApi: OpenApi[T]): Components[T] =
    openApi.components.getOrElse(Components(Map.empty, Map.empty, Map.empty, Map.empty))

  object Http {

    sealed trait Verb

    object Verb {
      final case object Put    extends Verb
      final case object Post   extends Verb
      final case object Get    extends Verb
      final case object Delete extends Verb

      def methodFrom: Verb => String = {
        case Post => "create"
        case Put  => "update"
        case x    => x.show.toLowerCase()
      }

      implicit val httpVerbShow: Show[Verb] = Show.show {
        case Put    => "PUT"
        case Post   => "POST"
        case Get    => "GET"
        case Delete => "DELETE"
      }
    }

    final case class Path(value: String) extends AnyVal
    object Path {

      private def splitPath(value: String): (List[String], List[String]) = {
        val (x, y) = value.split("/").partition(_.contains("{"))
        y.filter(_.nonEmpty)
          .reverse
          .map(normalize)
          .toList -> x.filter(_.nonEmpty).map(_.replaceAll("[{|}]", "")).reverse.map(normalize).toList
      }

      private def queryParams(value: String): List[String] =
        value.split("&").flatMap(_.split("=").headOption).toList.map(normalize)

      def splitParts(path: Path): (List[String], List[String], List[String]) =
        path.value.split("\\?").toList match {
          case Nil      => (Nil, Nil, Nil)
          case x :: Nil => un(second(splitPath(x))(_ -> List.empty[String]))
          case x :: xs  => un(second(splitPath(x))(_ -> queryParams(xs.mkString)))
        }
      implicit val httpPathShow: Show[Path] = Show.show(_.value)
    }

    final case class OperationId private (value: String) extends AnyVal

    object OperationId {

      def apply[T](verb: Verb, path: Path, operation: openapi.schema.Path.Operation[T]): OperationId = {
        val (paths, varPaths, queries) = Path.splitParts(path)
        val params                     = varPaths ++ queries
        OperationId(
          decapitalize(
            ident(
              operation.operationId
                .getOrElse {
                  val printVerb             = Http.Verb.methodFrom(verb)
                  val printParamsIfRequired = if (params.nonEmpty) s"By${params.mkString}" else ""
                  s"${printVerb}${paths.mkString}${printParamsIfRequired}"
                }
            )
          )
        )
      }

      implicit val operationIdShow: Show[OperationId] = Show.show(_.value)
    }

    case class Operation[T](
        verb: Http.Verb,
        path: Http.Path,
        description: Option[String],
        operationId: OperationId,
        parameters: List[Parameter[T]],
        requestBody: Option[Either[Request[T], Reference]],
        responses: Map[String, Either[Response[T], Reference]]
    )

    object Operation {
      def apply[T](
          verb: Http.Verb,
          path: Http.Path,
          operation: openapi.schema.Path.Operation[T],
          itemObject: ItemObject[T],
          components: Components[T]
      ): Http.Operation[T] = {

        def findParameter(name: String): Option[Parameter[T]] =
          components.parameters.get(name).flatMap(_.fold(_.some, parameterFrom))

        def parameterFrom(reference: Reference): Option[Parameter[T]] =
          reference.ref match {
            case parametersRegex(name) => findParameter(name)
            case name                  => findParameter(name)
          }
        val parameters = (itemObject.parameters ++ operation.parameters).flatMap(_.fold(_.some, parameterFrom)).distinct

        Http.Operation(
          verb,
          path,
          operation.description,
          Http.OperationId(verb, path, operation),
          parameters,
          operation.requestBody,
          operation.responses
        )
      }
    }
    type Responses[T] = (OperationId, Map[String, Either[Response[T], Reference]])
  }
  final case class TraitName(value: String) extends AnyVal
  object TraitName {
    def apply[T](openApi: OpenApi[T]): TraitName = TraitName(s"${normalize(openApi.info.title)}Client")
    implicit val traitNameShow: Show[TraitName]  = Show.show(_.value)
  }
  final case class ImplName(value: String) extends AnyVal
  object ImplName {
    def apply[T](openApi: OpenApi[T]): ImplName = ImplName(s"${normalize(openApi.info.title)}HttpClient")
    implicit val implNameShow: Show[ImplName]   = Show.show(_.value)
  }

  private def requestTuple[T: Basis[JsonSchemaF, *]](
      operationId: Http.OperationId,
      request: Request[T]
  ): Option[VarWithType[T]] =
    jsonFrom(request.content)
      .flatMap(
        _.schema.map(x =>
          VarWithType
            .tpe[T](Tpe(x, request.required, request.description.getOrElse(defaultRequestName(operationId)), Nil))
        )
      )

  private def referenceTuple[T: Basis[JsonSchemaF, *]](reference: Reference): Option[VarWithType[T]] =
    reference.ref match {
      case schemasRegex(name) => VarWithType.tpe(Tpe.apply(name)).some
      case _                  => none
    }

  def requestOrTuple[T: Basis[JsonSchemaF, *]](
      operationId: Http.OperationId,
      requestOr: Either[Request[T], Reference]
  ): Option[VarWithType[T]] =
    requestOr.fold[Option[VarWithType[T]]](requestTuple(operationId, _), referenceTuple)

  private def parameterTuple[T: Basis[JsonSchemaF, *]](x: Parameter[T]): VarWithType[T] =
    VarWithType(
      x.name,
      x.schema,
      x.required,
      x.name,
      if (isBasicType(x.schema) || isReference(x.schema)(schemasRegex)) Nil else List(parametersPackage)
    )

  private def operationTuple[T: Basis[JsonSchemaF, *]](
      operation: Http.Operation[T]
  ): (Http.OperationId, (List[VarWithType[T]]), Http.Responses[T]) =
    (
      operation.operationId,
      operation.parameters.map(parameterTuple[T]) ++
        operation.requestBody
          .flatMap(requestOrTuple[T](operation.operationId, _)),
      operation.operationId -> operation.responses
    )

  private def defaultRequestName(operationId: Http.OperationId): String =
    show"${operationId}Request".capitalize

  case class TypeAliasErrorResponse(operationId: Http.OperationId)

  object TypeAliasErrorResponse {
    implicit val typeAliasErrorResponse: Show[TypeAliasErrorResponse] =
      Show.show(x => show"${x.operationId}ErrorResponse".capitalize)
  }

  private def typeFromResponse[T: Basis[JsonSchemaF, *]](response: Response[T]): Option[T] =
    jsonFrom(response.content).flatMap(_.schema)

  private def responseType[T: Basis[JsonSchemaF, *]]: Printer[Response[T]] =
    tpe.contramap { response =>
      typeFromResponse(response)
        .map(Tpe(_, true, normalize(response.description), Nil))
        .getOrElse(Tpe.unit)
    }

  private def referenceTpe[T: Basis[JsonSchemaF, *]](x: Reference): Tpe[T] =
    referenceTuple(x).map(_.tpe).getOrElse(Tpe.unit)

  def responseOrType[T: Basis[JsonSchemaF, *]]: Printer[Either[Response[T], Reference]] =
    responseType >|< tpe.contramap(referenceTpe[T])

  def successType[T](responses: Map[String, Either[Response[T], Reference]]): Option[Either[Response[T], Reference]] =
    responses.find(x => successStatusCode(x._1)).map(_._2)

  def responsesTypes[T: Basis[JsonSchemaF, *]]: Printer[Http.Responses[T]] =
    Printer { case (x, y) =>
      val defaultName = TypeAliasErrorResponse(x)
      y.size match {
        case 0 => "Unit"
        case 1 => responseOrType.print(y.head._2)
        case _ =>
          val success = successType(y).fold("Unit")(responseOrType.print)
          show"Either[$defaultName, $success]"
      }
    }

  def method[T: Basis[JsonSchemaF, *]]: Printer[Http.Operation[T]] =
    (
      κ("  def ") *< show[Http.OperationId],
      κ("(") *< sepBy(argumentDef, ", "),
      κ("): F[") *< responsesTypes >* κ("]")
    ).contramapN(operationTuple[T])

  private def itemObjectTuple[T](
      thePath: String,
      itemObject: ItemObject[T],
      components: Components[T]
  ): List[Http.Operation[T]] = {
    val path = Http.Path.apply(thePath)

    def operation(verb: Http.Verb, operation: openapi.schema.Path.Operation[T]): Http.Operation[T] =
      Http.Operation(verb, path, operation, itemObject, components)

    List(
      itemObject.get.map(operation(Http.Verb.Get, _)),
      itemObject.delete.map(operation(Http.Verb.Delete, _)),
      itemObject.post.map(operation(Http.Verb.Post, _)),
      itemObject.put.map(operation(Http.Verb.Put, _))
    ).flatten
  }

  def typeAndSchemaFor[T: Basis[JsonSchemaF, *], X](
      operationId: Option[Http.OperationId],
      response: Response[T],
      tpe: String
  )(success: => (String, List[String]))(implicit codecs: Printer[Codecs]): (String, Option[String], List[String]) = {
    val innerType = s"${operationId.fold("")(_.show.capitalize)}${normalize(response.description).capitalize}"
    val newSchema =
      typeFromResponse(response).map(schema(innerType.some).print).getOrElse("Unit")
    tpe match {
      case _ if tpe === newSchema =>
        un(second(success)(x => none -> x))
      case _ if newSchema.nonEmpty => (tpe, innerType.some, List(newSchema))
      case _                       => (tpe, none, Nil)
    }
  }

  def defaultTypesAndSchemas[T: Basis[JsonSchemaF, *]](
      operationId: Http.OperationId,
      responseOr: Either[Response[T], Reference]
  )(implicit codecs: Printer[Codecs]): (String, String, List[String]) = {
    def unexpectedErrorName(operationId: Http.OperationId): String =
      show"${operationId}UnexpectedErrorResponse".capitalize
    val tpe     = responseOrType.print(responseOr)
    val newType = unexpectedErrorName(operationId)
    def statusCaseClass(tpe: String): String =
      caseClassDef.print(newType -> List("statusCode" -> Tpe[T]("Int"), "value" -> Tpe[T](tpe)))
    responseOr.fold(
      r => {
        val (_, anonymousType, schemas) = typeAndSchemaFor(operationId.some, r, tpe)(tpe -> List.empty)
        un(second(newType -> schemas) { x =>
          anonymousType.getOrElse(tpe) -> (x ++ List(statusCaseClass(anonymousType.getOrElse(tpe))))
        })
      },
      _ => (newType, tpe, List(statusCaseClass(tpe)))
    )
  }

  def statusTypesAndSchemas[T: Basis[JsonSchemaF, *]](
      operationId: Http.OperationId,
      responseOr: Either[Response[T], Reference]
  )(implicit codecs: Printer[Codecs]): (String, Option[String], List[String]) = {
    val tpe = responseOrType.print(responseOr)
    responseOr.left.toOption
      .map { response =>
        def defaultResponseErrorName(operationId: Http.OperationId, name: String): String =
          show"${operationId}${normalize(name).capitalize}Error".capitalize
        val (newTpe, anonymousType, schemas) = typeAndSchemaFor(operationId.some, response, tpe) {
          val newTpe = defaultResponseErrorName(operationId, response.description)
          newTpe -> List(caseClassDef.print(newTpe -> List("value" -> Tpe[T](tpe))))
        }
        (newTpe, anonymousType, schemas)
      }
      .getOrElse(
        (tpe, None, List.empty)
      )
  }

  def typesAndSchemas[T: Basis[JsonSchemaF, *]](operationId: Http.OperationId)(
      status: String,
      responseOr: Either[Response[T], Reference]
  )(implicit codecs: Printer[Codecs]): (String, List[String]) = {
    val statusCodePattern = """(\d+)""".r
    val tpe               = responseOrType.print(responseOr)
    status match {
      case statusCodePattern(code) if successStatusCode(code) =>
        responseOr.left.toOption
          .map { response =>
            val (newTpe, _, schemas) = typeAndSchemaFor(none, response, tpe) {
              tpe -> Nil
            }
            newTpe -> schemas
          }
          .getOrElse(tpe -> List.empty)

      case statusCodePattern(_) =>
        val (x, a, y) = statusTypesAndSchemas(operationId, responseOr)
        a.getOrElse(x) -> y

      case "default" =>
        val (x, _, y) = defaultTypesAndSchemas(operationId, responseOr)
        x -> y
    }
  }

  private def responsesSchemaTuple[T: Basis[JsonSchemaF, *]](
      operationId: Http.OperationId,
      responses: Map[String, Either[Response[T], Reference]]
  )(implicit codecs: Printer[Codecs]): Either[(List[String], TypeAliasErrorResponse, List[String]), List[String]] =
    responses.toList match {
      case (_, response) :: Nil =>
        response.left.toOption
          .map(_ -> responseOrType.print(response))
          .toList
          .flatMap { case (response, tpe) =>
            typeAndSchemaFor(none, response, tpe)(tpe -> Nil)._3
          }
          .asRight
      case _ =>
        val (newTypes, schemas) =
          second(responses.map((typesAndSchemas[T](operationId) _).tupled).toList.unzip)(_.flatten)
        (
          schemas.toList.filter(_.nonEmpty),
          TypeAliasErrorResponse(operationId),
          newTypes.toList.filterNot(_.some === successType(responses).map(responseOrType[T].print))
        ).asLeft
    }

  private def responseErrorsDef: Printer[List[String]] =
    (sepBy(string, " :+: "), (κ(" :+: CNil") >|< unit)).contramapN(errorTypes =>
      (errorTypes, if (errorTypes.size > 1) ().asLeft else ().asRight)
    )

  private def multipleResponsesSchema: Printer[(List[String], TypeAliasErrorResponse, List[String])] = {
    import Printer.avoid._
    (
      sepBy((space >* space) *< string, "\n") >* newLine,
      space *< space *< typeAliasDef(responseErrorsDef)
    ).contramapN { case (schemas, tpe, errorTypes) =>
      (schemas, (tpe.show, errorTypes, none))
    }
  }

  private def responsesSchema[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs]
  ): Printer[(Http.OperationId, Map[String, Either[Response[T], Reference]])] =
    (multipleResponsesSchema >|< sepBy((space >* space) *< string, "\n")).contramap((responsesSchemaTuple[T] _).tupled)

  private def requestSchemaTuple[T: Basis[JsonSchemaF, *]](
      operationId: Http.OperationId,
      request: Option[Either[Request[T], Reference]]
  ): Option[(String, T)] =
    for {
      request <- request.flatMap(_.left.toOption)
      requestTuple <- requestTuple(operationId, request)
        .map(_.tpe)
      requestTpe <- requestTuple.tpe.toOption
      name    = tpe.print(requestTuple)
      newType = defaultRequestName(operationId)
      if name === newType
    } yield (newType -> requestTpe)

  private def requestSchema[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs]
  ): Printer[(Http.OperationId, Option[Either[Request[T], Reference]])] =
    (optional((space >* space) *< schemaWithName[T])).contramap((requestSchemaTuple[T] _).tupled)

  private def parameterSchema[T: Basis[JsonSchemaF, *]](implicit codecs: Printer[Codecs]): Printer[Parameter[T]] =
    schemaWithName[T].contramap(x => x.name -> x.schema)

  private def clientTypes[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs]
  ): Printer[(List[Http.Operation[T]], List[Parameter[T]])] =
    (
      sepBy(requestSchema[T], "\n") >* newLine,
      sepBy(responsesSchema[T], "\n") >* newLine,
      optional(objectDef(sepBy(parameterSchema[T], "\n")))
    ).contramapN { case (ops, params) =>
      (
        ops.map(operation => operation.operationId -> operation.requestBody),
        ops.map(operation => operation.operationId -> operation.responses),
        params.headOption.map(_ => ((parametersPackage, none), List.empty, params))
      )
    }

  def toOperationsWithPath[T](
      traitName: TraitName,
      itemObjects: Map[String, ItemObject[T]],
      components: Components[T]
  ): (TraitName, List[Http.Operation[T]]) =
    traitName -> itemObjects.toList.flatMap { case (n, i) => itemObjectTuple[T](n, i, components) }

  private def operationsTuple[T: Basis[JsonSchemaF, *]](openApi: OpenApi[T]): (
      List[PackageName],
      TraitName,
      TraitName,
      List[Http.Operation[T]],
      ((String, Option[String]), List[PackageName], (List[Http.Operation[T]], List[Parameter[T]]))
  ) = {
    val traitName = TraitName(openApi)
    val (a, b, c, d) = un(second(duplicate(toOperationsWithPath(traitName, openApi.paths, componentsFrom(openApi)))) {
      case (a, b) => (a, (traitName, b))
    })
    val parameters = d._2.flatMap(_.parameters).distinct.filterNot { x =>
      isBasicType(x.schema) || isReference(x.schema)(schemasRegex)
    }
    (
      List("models._", "shapeless.{:+:, CNil}").map(PackageName.apply),
      a,
      b,
      c,
      ((d._1.show, none), List.empty, (d._2, parameters))
    )
  }

  def interfaceDefinition[T: Basis[JsonSchemaF, *]](implicit codecs: Printer[Codecs]): Printer[OpenApi[T]] =
    optional(
      (
        sepBy(importDef, "\n") >* newLine,
        κ("trait ") *< show[TraitName] >* κ("[F[_]] {") >* newLine,
        space *< space *< κ("import ") *< show[TraitName] >* κ("._") >* newLine,
        sepBy(method[T], "\n") >* (newLine >* κ("}") >* newLine),
        objectDef(clientTypes[T] >* newLine)
      ).contramapN(operationsTuple[T])
    ).contramap(x => x.paths.toList.headOption.map(_ => x))

}
