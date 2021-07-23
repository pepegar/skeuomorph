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
import cats.kernel.Eq
import higherkindness.skeuomorph.openapi.schema.Path.ItemObject

/**
 * @see https://swagger.io/specification/
 */
object schema {
  final case class Reference(ref: String)
  final case class OpenApi[A](
      openapi: String,
      info: Info,
      servers: List[Server],
      paths: Map[String, Path.ItemObject[A]],
      components: Option[Components[A]],
      tags: List[Tag],
      externalDocs: Option[ExternalDocs]
  )

  object OpenApi {
    import higherkindness.droste._
    import Optimize._
    import cats.syntax.all._
    import cats.data.State

    implicit def openApiEq[T]: Eq[OpenApi[T]] =
      Eq.fromUniversalEquals

    private def withSchemas[T](openApi: OpenApi[T])(models: Map[String, T]): OpenApi[T] = {
      openApi.copy(
        components = openApi.components
          .fold(
            if (models.nonEmpty)
              Components[T](
                schemas = models,
                responses = Map.empty,
                requestBodies = Map.empty,
                parameters = Map.empty
              ).some
            else none
          )(x => x.copy(x.schemas ++ models).some)
      )
    }

    private def extractTypesFromMediaType[T: Basis[JsonSchemaF, *]](
        mediaType: MediaType[T]
    ): NestedTypesState[T, MediaType[T]] =
      mediaType.schema.traverse(nestedTypes.apply).map(x => mediaType.copy(schema = x))
    private def extractTypesFromContent[T: Basis[JsonSchemaF, *]](
        content: Map[String, MediaType[T]]
    ): NestedTypesState[T, Map[String, MediaType[T]]] =
      content.toList
        .traverse { case (x, m) => extractTypesFromMediaType[T](m).map(t => x -> t) }
        .map(_.toMap)

    private def extractTypesFromRequest[T: Basis[JsonSchemaF, *]](
        request: Either[Request[T], Reference]
    ): NestedTypesState[T, Either[Request[T], Reference]] =
      request.fold(
        x => extractTypesFromContent(x.content).map(y => x.copy(content = y).asLeft),
        x => State.pure(x.asRight)
      )

    private def extractTypesFromResponse[T: Basis[JsonSchemaF, *]](
        response: Either[Response[T], Reference]
    ): NestedTypesState[T, Either[Response[T], Reference]] =
      response.fold(
        x => extractTypesFromContent(x.content).map(y => x.copy(content = y).asLeft),
        x => State.pure(x.asRight)
      )

    private def extractTypesFormOperation[T: Basis[JsonSchemaF, *]](
        operation: Path.Operation[T]
    ): NestedTypesState[T, Path.Operation[T]] =
      for {
        requestBody <- operation.requestBody.traverse(extractTypesFromRequest[T])
        responses <-
          operation.responses.toList
            .traverse { case (x, r) => extractTypesFromResponse(r).map(t => x -> t) }
            .map(_.toMap)
      } yield operation.copy(
        requestBody = requestBody,
        responses = responses
      )

    private def extractTypesFromItemObject[T: Basis[JsonSchemaF, *]](
        itemObject: ItemObject[T]
    ): NestedTypesState[T, ItemObject[T]] =
      for {
        get    <- itemObject.get.traverse(extractTypesFormOperation[T])
        post   <- itemObject.post.traverse(extractTypesFormOperation[T])
        delete <- itemObject.delete.traverse(extractTypesFormOperation[T])
        put    <- itemObject.put.traverse(extractTypesFormOperation[T])
      } yield itemObject.copy(
        get = get,
        post = post,
        delete = delete,
        put = put
      )
    private def extractTypesFromComponents[T: Basis[JsonSchemaF, *]](
        components: Components[T]
    ): NestedTypesState[T, Components[T]] =
      components.schemas.toList
        .traverse { case (name, tpe) =>
          nestedTypes.apply(tpe).map(t => name -> t)
        }
        .map(cc => components.copy(schemas = cc.toMap))

    private def extractTypesFromPath[T: Basis[JsonSchemaF, *]](
        paths: Map[String, Path.ItemObject[T]]
    ): NestedTypesState[T, Map[String, Path.ItemObject[T]]] =
      paths.toList
        .traverse { case (x, i) => extractTypesFromItemObject(i).map(z => x -> z) }
        .map(_.toMap)

    def extractNestedTypes[T: Basis[JsonSchemaF, *]](openApi: OpenApi[T]): OpenApi[T] = {
      val state = for {
        components <- openApi.components.traverse(extractTypesFromComponents[T])
        paths      <- extractTypesFromPath(openApi.paths)
      } yield openApi.copy(
        components = components,
        paths = paths
      )
      val ((y, _), newOpenApi) = state.run(Map.empty[String, T] -> 0).value
      withSchemas(newOpenApi)(y)
    }
  }

  final case class Info(title: String, description: Option[String], version: String)

  final case class Server(url: String, description: Option[String], variables: Map[String, Server.Variable])

  object Server {
    final case class Variable(enum: List[String], default: String, description: Option[String])
  }

  object Path {
    final case class ItemObject[A](
        ref: Option[String], // $ref
        summary: Option[String],
        description: Option[String],
        get: Option[Operation[A]],
        put: Option[Operation[A]],
        post: Option[Operation[A]],
        delete: Option[Operation[A]],
        options: Option[Operation[A]],
        head: Option[Operation[A]],
        patch: Option[Operation[A]],
        trace: Option[Operation[A]],
        servers: List[Server],
        parameters: List[Either[Parameter[A], Reference]]
    )

    final case class Operation[A](
        tags: List[String],
        summary: Option[String],
        description: Option[String],
        externalDocs: Option[ExternalDocs],
        operationId: Option[String],
        parameters: List[Either[Parameter[A], Reference]],
        requestBody: Option[Either[Request[A], Reference]],
        responses: Map[String, Either[Response[A], Reference]],
        callbacks: Map[String, Either[Callback[A], Reference]],
        deprecated: Boolean,
        servers: List[Server]
    )
  }
  final case class Components[A](
      schemas: Map[String, A],
      responses: Map[String, Either[Response[A], Reference]],
      requestBodies: Map[String, Either[Request[A], Reference]],
      parameters: Map[String, Either[Parameter[A], Reference]]
  )

  final case class Request[A](description: Option[String], content: Map[String, MediaType[A]], required: Boolean)

  final case class MediaType[A](schema: Option[A], encoding: Map[String, Encoding[A]])

  final case class Encoding[A](
      contentType: Option[String],
      headers: Map[String, Either[Header[A], Reference]],
      style: Option[String],
      explode: Option[Boolean],
      allowReserved: Option[Boolean]
  )

  final case class Response[A](
      description: String,
      headers: Map[String, Either[Header[A], Reference]],
      content: Map[String, MediaType[A]]
  )

  final case class Tag(name: String, description: Option[String], externalDocs: Option[ExternalDocs])

  final case class ExternalDocs(url: String, description: Option[String])

  type Callback[A] = Map[String, Path.ItemObject[A]]

  object Callback {
    def apply[A](values: (String, Path.ItemObject[A])*): Callback[A] =
      values.toMap

  }

  final case class Header[A](description: String, schema: A)

  sealed trait Parameter[A] extends Product with Serializable {
    def name: String
    def in: Location
    def description: Option[String]
    def required: Boolean
    def deprecated: Boolean
    def style: String
    def explode: Boolean
    def allowEmptyValue: Boolean
    def allowReserved: Boolean
    def schema: A
  }

  object Parameter {

    def apply[A](
        name: String,
        in: Location,
        description: Option[String],
        required: Option[Boolean],
        deprecated: Option[Boolean],
        style: Option[String],
        explode: Option[Boolean],
        allowEmptyValue: Option[Boolean],
        allowReserved: Option[Boolean],
        schema: A
    ): Parameter[A] =
      in match {
        case Location.Path =>
          Path(
            name,
            description,
            deprecated.getOrElse(false),
            style.getOrElse("simple"),
            explode.getOrElse(false),
            schema
          )
        case Location.Query =>
          Query(
            name,
            description,
            required.getOrElse(false),
            deprecated.getOrElse(false),
            style.getOrElse("form"),
            allowEmptyValue.getOrElse(false),
            explode.getOrElse(true),
            allowReserved.getOrElse(false),
            schema
          )
        case Location.Header =>
          Header(
            name,
            description,
            required.getOrElse(false),
            deprecated.getOrElse(false),
            style.getOrElse("simple"),
            explode.getOrElse(false),
            schema
          )
        case Location.Cookie =>
          Cookie(
            name,
            description,
            required.getOrElse(false),
            deprecated.getOrElse(false),
            style.getOrElse("form"),
            explode.getOrElse(false),
            schema
          )
      }

    final case class Path[A](
        name: String,
        description: Option[String],
        deprecated: Boolean = false,
        style: String = "simple",
        explode: Boolean = false,
        schema: A
    ) extends Parameter[A] {
      val in: Location             = Location.Path
      val required: Boolean        = true
      val allowEmptyValue: Boolean = false
      val allowReserved: Boolean   = false
    }

    final case class Query[A](
        name: String,
        description: Option[String],
        required: Boolean = false,
        deprecated: Boolean = false,
        style: String = "form",
        allowEmptyValue: Boolean,
        explode: Boolean = true,
        allowReserved: Boolean = false,
        schema: A
    ) extends Parameter[A] {
      val in: Location = Location.Query
    }

    final case class Header[A](
        name: String,
        description: Option[String],
        required: Boolean = false,
        deprecated: Boolean = false,
        style: String = "simple",
        explode: Boolean = false,
        schema: A
    ) extends Parameter[A] {
      val in: Location             = Location.Header
      val allowEmptyValue: Boolean = false
      val allowReserved: Boolean   = false
    }

    final case class Cookie[A](
        name: String,
        description: Option[String],
        required: Boolean = false,
        deprecated: Boolean = false,
        style: String = "form",
        explode: Boolean = false,
        schema: A
    ) extends Parameter[A] {
      val in: Location             = Location.Cookie
      val allowEmptyValue: Boolean = false
      val allowReserved: Boolean   = false
    }

  }

  sealed abstract class Location(val value: String) extends Product with Serializable
  object Location {
    import cats.syntax.all._
    case object Path   extends Location("path")
    case object Query  extends Location("query")
    case object Header extends Location("header")
    case object Cookie extends Location("cookie")

    def all = List(Path, Query, Header, Cookie)

    def parse(value: String): Either[String, Location] =
      value match {
        case "path"   => Path.asRight
        case "query"  => Query.asRight
        case "header" => Header.asRight
        case "cookie" => Cookie.asRight
        case _        => s"$value is not valid location".asLeft
      }
  }

}
