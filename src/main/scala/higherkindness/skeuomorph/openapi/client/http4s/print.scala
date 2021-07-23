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

package higherkindness.skeuomorph.openapi.client.http4s

import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.Printer.{konst => κ, _}
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi._

import higherkindness.skeuomorph.openapi.print.{schema => _, _}
import higherkindness.skeuomorph.openapi.client.print._
import cats.syntax.all._
import higherkindness.droste._

object print {
  import schema._
  final case class EntityCodecs[T](name: String, tpe: T)

  def impl[T: Basis[JsonSchemaF, *]](implicit
      http4sSpecifics: Http4sSpecifics,
      codecs: Printer[Codecs]
  ): Printer[(PackageName, OpenApi[T])] =
    optional(divBy(sepBy(importDef, "\n"), newLine, implDefinition[T])).contramap { case (packageName, openApi) =>
      openApi.paths.toList.headOption.map(_ =>
        packages ++ (List(
          s"${packageName.show}.${TraitName(openApi).show}",
          s"${packageName.show}.models._"
        ) ++ sumTypes(openApi).map(x => s"$x._")).map(PackageName.apply) ->
          openApi
      )
    }

  val listEncoderPrinter: Printer[Unit] = κ(
    "implicit def listEntityEncoder[T: Encoder]: EntityEncoder[F, List[T]] = jsonEncoderOf[F, List[T]]"
  )
  val listDecoderPrinter: Printer[Unit] = κ(
    "implicit def listEntityDecoder[T: Decoder]: EntityDecoder[F, List[T]] = jsonOf[F, List[T]]"
  )
  val optionListEncoderPrinter: Printer[Unit] = κ(
    "implicit def optionListEntityEncoder[T: Encoder]: EntityEncoder[F, Option[List[T]]] = jsonEncoderOf[F, Option[List[T]]]"
  )
  val optionListDecoderPrinter: Printer[Unit] = κ(
    "implicit def optionListEntityDecoder[T: Decoder]: EntityDecoder[F, Option[List[T]]] = jsonOf[F, Option[List[T]]]"
  )
  val timeQueryParamEncoder: Printer[Unit] =
    κ(
      "localDateTimeQueryEncoder: QueryParamEncoder[java.time.LocalDateTime], localDateQueryEncoder: QueryParamEncoder[java.time.LocalDate]"
    )
  val showQueryParamEncoder: Printer[Unit] =
    κ(
      "implicit def showQueryParameter[T: Show]: QueryParamEncoder[T] = QueryParamEncoder.stringQueryParamEncoder.contramap(_.show)"
    )
  type ImplDef[T] = (TraitName, TraitName, List[PackageName], List[Http.Operation[T]], (TraitName, ImplName))
  def implDefinition[T: Basis[JsonSchemaF, *]](implicit
      http4sSpecifics: Http4sSpecifics,
      codecs: Printer[Codecs]
  ): Printer[OpenApi[T]] =
    optional(
      objectDef[ImplDef[T]](
        (
          κ("  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)") *< κ(
            "(implicit "
          ) *< timeQueryParamEncoder *< κ(
            ")"
          ) *< κ(": ") *< show[TraitName] >* κ("[F]"),
          κ(" = new ") *< show[TraitName] >* κ("[F] {") >* newLine,
          twoSpaces *< twoSpaces *< sepBy(importDef, "\n") >* newLine *<
            twoSpaces *< twoSpaces *< listEncoderPrinter *< newLine *<
            twoSpaces *< twoSpaces *< listDecoderPrinter *< newLine *<
            twoSpaces *< twoSpaces *< optionListEncoderPrinter *< newLine *<
            twoSpaces *< twoSpaces *< optionListDecoderPrinter *< newLine *<
            twoSpaces *< twoSpaces *< showQueryParamEncoder *< newLine,
          sepBy(twoSpaces *< methodImpl, "\n") >* newLine *< κ("  }") *< newLine,
          http4sSpecifics.applyMethod
        ).contramapN(identity)
      )
    ).contramap { x =>
      x.paths.toList.headOption.map(_ =>
        (
          (ImplName(x).show, none),
          List.empty,
          (
            TraitName(x),
            TraitName(x),
            List(PackageName(s"${TraitName(x).show}._")),
            toOperationsWithPath(TraitName(x), x.paths, componentsFrom(x))._2,
            TraitName(x) -> ImplName(x)
          )
        )
      )
    }

  def successResponseImpl[T: Basis[JsonSchemaF, *]]: Printer[Either[Response[T], Reference]] =
    (κ("case Successful(response) => response.as[") *< responseOrType[T] >* κ("].map(_.asRight)"))
      .contramap(identity)

  def coproduct[A](printer: Printer[A]): Printer[(String, A)] =
    (κ("Coproduct[") *< string >* κ("]"), κ("(") *< printer >* κ(")")).contramapN(identity)

  def coproductIf[A](printer: Printer[A]): Printer[(String, A, Int)] =
    (printer >|< coproduct(printer)).contramap {
      case (x, y, numberOfErrors) if numberOfErrors > 1 => (x -> y).asRight
      case (_, y, _)                                    => y.asLeft
    }

  def statusResponseImpl[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs]
  ): Printer[(Http.OperationId, String, (Either[Response[T], Reference], Int))] =
    (
      κ("case response if response.status.code == ") *< string >* κ(" => "),
      κ("response.as[") *< string >* κ("]"),
      κ(".map(x => ") *< coproductIf(string >* κ("(x)") >|< κ("x")) >* κ(".asLeft)")
    ).contramapN { case (operationId, status, (r, n)) =>
      val (tpe, anonymousType, _) = statusTypesAndSchemas[T](operationId, r)
      val responseTpe             = anonymousType.getOrElse(responseOrType.print(r))
      (
        status,
        responseTpe,
        (
          TypeAliasErrorResponse(operationId).show,
          anonymousType.fold[Either[String, Unit]](if (tpe === responseTpe) ().asRight else tpe.asLeft)(_ =>
            ().asRight
          ),
          n
        )
      )
    }

  def defaultResponseImpl[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs]
  ): Printer[(Http.OperationId, (Either[Response[T], Reference], Int))] =
    (
      κ("case default => default.as[") *< string >* κ("]"),
      κ(".map(x => ") *< coproductIf(string >* κ("(default.status.code, x)")) >* κ(".asLeft)")
    ).contramapN { case (operationId, (x, n)) =>
      val (tpe, innerTpe, _) = defaultTypesAndSchemas(operationId, x)
      (innerTpe, (TypeAliasErrorResponse(operationId).show, tpe, n))
    }

  def responseImpl[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs]
  ): Printer[(Http.OperationId, String, (Either[Response[T], Reference], Int))] =
    (successResponseImpl[T] >|< defaultResponseImpl[T] >|< statusResponseImpl[T]).contramap {
      case (_, statusCodePattern(code), (x, _)) if successStatusCode(code) => (x.asLeft).asLeft
      case (operationId, "default", x)                                     => (operationId -> x).asRight.asLeft
      case x                                                               => x.asRight
    }

  private def queryParametersFrom[T]: Http.Operation[T] => List[Parameter.Query[T]] =
    _.parameters.collect { case x: Parameter.Query[T] =>
      x
    }
  private def headerParametersFrom[T]: Http.Operation[T] => List[Parameter.Header[T]] =
    _.parameters.collect { case x: Parameter.Header[T] =>
      x
    }

  def requestImpl[T: Basis[JsonSchemaF, *]](implicit http4sSpecifics: Http4sSpecifics): Printer[Http.Operation[T]] =
    (
      κ("Request[F](method = Method.") *< show[Http.Verb],
      κ(", uri = baseUrl ") *< divBy(httpPath[T], unit, sepBy(queryParameter[T], "")) >* κ(")"),
      optional(http4sSpecifics.withBody),
      optional(http4sSpecifics.withHeaders[T])
    ).contramapN { x =>
      val headers = headerParametersFrom(x)
      (
        x.verb,
        x.path -> queryParametersFrom(x),
        x.requestBody.flatMap(requestOrTuple[T](x.operationId, _)).map(_.name),
        headers.headOption.map(_ => headers)
      )
    }

  def fetchImpl[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs],
      http4sSpecifics: Http4sSpecifics
  ): Printer[Http.Operation[T]] =
    (
      κ("fetch[") *< responsesTypes >* κ("]("),
      requestImpl[T] >* κ(") {") >* newLine,
      sepBy(twoSpaces *< twoSpaces *< twoSpaces *< responseImpl[T], "\n") >* newLine,
      twoSpaces *< twoSpaces *< κ("}")
    ).contramapN { operation =>
      (
        (operation.operationId -> operation.responses),
        operation,
        operation.responses.toList.map { x =>
          un(
            operation.operationId -> second(x)(y =>
              y -> operation.responses.filterNot { case (s, _) => successStatusCode(s) }.size
            )
          )
        },
        ()
      )
    }

  def expectImpl[T: Basis[JsonSchemaF, *]](implicit http4sSpecifics: Http4sSpecifics): Printer[Http.Operation[T]] =
    (κ("expect[") *< responsesTypes >* κ("]("), requestImpl[T] >* κ(")"))
      .contramapN(x => (x.operationId -> x.responses, x))

  def methodImpl[T: Basis[JsonSchemaF, *]](implicit
      codecs: Printer[Codecs],
      http4sSpecifics: Http4sSpecifics
  ): Printer[Http.Operation[T]] =
    (
      method[T] >* κ(" = client."),
      expectImpl[T] >|< fetchImpl[T]
    ).contramapN(x => (x, if (x.responses.size > 1) x.asRight else x.asLeft))

  def queryParameter[T]: Printer[Parameter.Query[T]] =
    (space *< string >* space, κ("(\"") *< divBy(string, κ("\", "), show[Var]) >* κ(")")).contramapN { q =>
      def op(x: Parameter.Query[T]): String = if (x.required) "+?" else "+??"
      (op(q), q.name -> Var(q.name))
    }

  def httpPath[T]: Printer[Http.Path] =
    (κ("/ ") *< sepBy((string >|< (show[Var] >* κ(".show"))), " / ")).contramap {
      _.show
        .split("/")
        .filter(_.nonEmpty)
        .map { s =>
          if (s.startsWith("{") && s.endsWith("}"))
            Var(s.tail.init).asRight
          else
            ("\"" + s + "\"").asLeft
        }
        .toList

    }

  private val packages = List(
    "cats._",
    "cats.effect._",
    "cats.implicits._",
    "io.circe._",
    "org.http4s._",
    "org.http4s.client.Client",
    "org.http4s.client.blaze._",
    "org.http4s.circe._",
    "org.http4s.Status.Successful",
    "shapeless.Coproduct",
    "scala.concurrent.ExecutionContext"
  ).map(PackageName.apply)

  private val twoSpaces: Printer[Unit] = space *< space

  trait Http4sSpecifics {
    def applyMethod: Printer[(TraitName, ImplName)]
    def withBody: Printer[Var]
    def withHeaders[T]: Printer[List[Parameter.Header[T]]]
  }
  object Http4sSpecifics {
    def apply(implicit http4sSpecifics: Http4sSpecifics): Http4sSpecifics = http4sSpecifics

    def header[T]: Printer[Parameter.Header[T]] =
      divBy(κ("Header(\"") *< string, κ("\", "), show[Var] >* κ(".show)")).contramap(x => x.name -> Var(x.name))
  }
  object v20 {
    implicit val v20Http4sSpecifics: Http4sSpecifics = new Http4sSpecifics {

      def applyMethod: Printer[(TraitName, ImplName)] =
        divBy(
          κ(
            "  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext, "
          ) *< timeQueryParamEncoder *< κ(
            "): Resource[F, "
          ) *< show[TraitName] >* κ("[F]] ="),
          space,
          κ("BlazeClientBuilder(executionContext).resource.map(") *< show[ImplName] >* κ(".build(_, baseUrl))")
        )

      def withBody: Printer[Var] =
        κ(".withEntity(") *< show[Var] >* κ(")")

      def withHeaders[T]: Printer[List[Parameter.Header[T]]] =
        κ(".withHeaders(Headers.of(") *< sepBy(Http4sSpecifics.header[T], ", ") >* κ("))")
    }
  }
  object v18 {
    implicit val v18Http4sSpecifics: Http4sSpecifics = new Http4sSpecifics {
      def applyMethod: Printer[(TraitName, ImplName)] =
        divBy(
          κ(
            "  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext, "
          ) *< timeQueryParamEncoder *< κ(
            "): F["
          ) *< show[TraitName] >* κ("[F]] ="),
          space,
          κ(
            "Http1Client[F](config = BlazeClientConfig.defaultConfig.copy(executionContext = executionContext)).map("
          ) *< show[
            ImplName
          ] >* κ(".build(_, baseUrl))")
        )

      def withBody: Printer[Var] =
        κ(".withBody(") *< show[Var] >* κ(")")

      def withHeaders[T]: Printer[List[Parameter.Header[T]]] =
        κ(".withHeaders(Headers(") *< sepBy(Http4sSpecifics.header[T], ", ") >* κ("))")
    }
  }
}
