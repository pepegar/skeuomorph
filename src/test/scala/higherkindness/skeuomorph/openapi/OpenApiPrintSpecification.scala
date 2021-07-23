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
import higherkindness.skeuomorph.Printer
import higherkindness.skeuomorph.openapi.schema.{Parameter, Reference}
import cats.syntax.all._

class OpenApiPrintSpecification extends org.specs2.mutable.Specification {
  import JsonSchemaF.Fixed
  import print._
  import helpers._
  import OpenApiPrintSpecification._

  "models should able to print" >> {
    "when not types are provided" >> {
      import client.http4s.circe._
      model.print(petstoreOpenApi) must ===("")
    }
    "when a basic type is provided" >> {
      import client.http4s.circe._
      model.print(petstoreOpenApi.withSchema("Foo", Fixed.string())) must
        ===(s"""|object models {
                |$modelImports
                |type Foo = String
                |}""".stripMargin)
    }

    "when a object type is provided" >> {
      import client.http4s.circe._
      model.print(petstoreOpenApi.withSchema("Bar", obj("foo" -> Fixed.string())())) must ===(s"""|object models {
            |$modelImports
            |final case class Bar(foo: Option[String])
            |object Bar {
            |
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import org.http4s.{EntityEncoder, EntityDecoder}
            |  import org.http4s.circe._
            |  import cats.Applicative
            |  import cats.effect.Sync
            |  implicit val BarEncoder: Encoder[Bar] = deriveEncoder[Bar]
            |  implicit val BarDecoder: Decoder[Bar] = deriveDecoder[Bar]
            |  implicit def BarEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Bar] = jsonEncoderOf[F, Bar]
            |  implicit def OptionBarEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[Bar]] = jsonEncoderOf[F, Option[Bar]]
            |  implicit def BarEntityDecoder[F[_]:Sync]: EntityDecoder[F, Bar] = jsonOf[F, Bar]
            |
            |}
            |}""".stripMargin)
    }

    "when a object type is provided with invalid identifier names" >> {
      import client.http4s.circe._
      model.print(
        petstoreOpenApi
          .withSchema("212bar_Foo-X1", obj("1fo_o" -> Fixed.string(), "ba-r" -> Fixed.integer())())
      ) must ===(s"""|object models {
            |$modelImports
            |final case class `212bar_Foo-X1`(`1fo_o`: Option[String], `ba-r`: Option[Int])
            |object `212bar_Foo-X1` {
            |
            |  import io.circe._
            |  import io.circe.generic.semiauto._
            |  import org.http4s.{EntityEncoder, EntityDecoder}
            |  import org.http4s.circe._
            |  import cats.Applicative
            |  import cats.effect.Sync
            |  implicit val BarFooX1212Encoder: Encoder[`212bar_Foo-X1`] = Encoder.forProduct2("1fo_o", "ba-r")(t => (t.`1fo_o`, t.`ba-r`))
            |  implicit val BarFooX1212Decoder: Decoder[`212bar_Foo-X1`] = Decoder.forProduct2("1fo_o", "ba-r")(`212bar_Foo-X1`.apply)
            |  implicit def BarFooX1212EntityEncoder[F[_]:Applicative]: EntityEncoder[F, `212bar_Foo-X1`] = jsonEncoderOf[F, `212bar_Foo-X1`]
            |  implicit def OptionBarFooX1212EntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[`212bar_Foo-X1`]] = jsonEncoderOf[F, Option[`212bar_Foo-X1`]]
            |  implicit def BarFooX1212EntityDecoder[F[_]:Sync]: EntityDecoder[F, `212bar_Foo-X1`] = jsonOf[F, `212bar_Foo-X1`]
            |
            |}
            |}""".stripMargin)
    }

    "when an array is provided" >> {
      import client.http4s.circe._
      model.print(
        petstoreOpenApi.withSchema(
          "Bars",
          Fixed.array(Fixed.reference("#/components/schemas/Bar"))
        )
      ) must ===(s"""|object models {
              |$modelImports
              |type Bars = List[Bar]
              |}""".stripMargin)
    }

    "when a array type is provided with invalid identifier names" >> {
      import Printer.avoid._
      model.print(
        petstoreOpenApi.withSchema(
          "bar_Foo-X1s",
          Fixed.array(Fixed.reference("#/components/schemas/bar_Foo-X1"))
        )
      ) must ===(s"""|object models {
              |$modelImports
              |type `bar_Foo-X1s` = List[`bar_Foo-X1`]
              |}""".stripMargin)
    }

    "when enum is provided" >> {
      import client.http4s.circe._
      model.print(petstoreOpenApi.withSchema("Color", Fixed.enum(List("Blue", "Red")))) must ===(
        s"""|object models {
            |$modelImports
            |sealed trait Color
            |object Color {
            |
            |  final case object Blue extends Color
            |  final case object Red extends Color
            |  import org.http4s.{EntityEncoder, EntityDecoder}
            |  import org.http4s.circe._
            |  import cats.Applicative
            |  import cats.effect.Sync
            |  import cats._
            |  import cats.syntax.all._
            |  import io.circe._
            |  implicit val ColorShow: Show[Color] = Show.show {
            |  case Blue => "Blue"
            |  case Red => "Red"
            |}
            |  implicit val ColorEncoder: Encoder[Color] = Encoder.encodeString.contramap(_.show)
            |  implicit val ColorDecoder: Decoder[Color] = Decoder.decodeString.emap {
            |  case "Blue" => Blue.asRight
            |  case "Red" => Red.asRight
            |  case x => s"$$x is not valid Color".asLeft
            |}
            |
            |  implicit def ColorEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Color] = jsonEncoderOf[F, Color]
            |  implicit def OptionColorEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[Color]] = jsonEncoderOf[F, Option[Color]]
            |  implicit def ColorEntityDecoder[F[_]:Sync]: EntityDecoder[F, Color] = jsonOf[F, Color]
            |
            |}
            |}""".stripMargin
      )
    }

    "when enum is provided with invalid identifier names" >> {
      import client.http4s.circe._
      model.print(petstoreOpenApi.withSchema("something-For", Fixed.enum(List("xo-m", "yy-y")))) must ===(
        s"""|object models {
             |$modelImports
             |sealed trait `something-For`
             |object `something-For` {
             |
             |  final case object `xo-m` extends `something-For`
             |  final case object `yy-y` extends `something-For`
             |  import org.http4s.{EntityEncoder, EntityDecoder}
             |  import org.http4s.circe._
             |  import cats.Applicative
             |  import cats.effect.Sync
             |  import cats._
             |  import cats.syntax.all._
             |  import io.circe._
             |  implicit val SomethingForShow: Show[`something-For`] = Show.show {
             |  case `xo-m` => "xo-m"
             |  case `yy-y` => "yy-y"
             |}
             |  implicit val SomethingForEncoder: Encoder[`something-For`] = Encoder.encodeString.contramap(_.show)
             |  implicit val SomethingForDecoder: Decoder[`something-For`] = Decoder.decodeString.emap {
             |  case "xo-m" => `xo-m`.asRight
             |  case "yy-y" => `yy-y`.asRight
             |  case x => s"$$x is not valid `something-For`".asLeft
             |}
             |
             |  implicit def SomethingForEntityEncoder[F[_]:Applicative]: EntityEncoder[F, `something-For`] = jsonEncoderOf[F, `something-For`]
             |  implicit def OptionSomethingForEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[`something-For`]] = jsonEncoderOf[F, Option[`something-For`]]
             |  implicit def SomethingForEntityDecoder[F[_]:Sync]: EntityDecoder[F, `something-For`] = jsonOf[F, `something-For`]
             |
             |}
             |}""".stripMargin
      )
    }

    "when a sum type is provided" >> {

      import client.http4s.circe._
      model.print(
        petstoreOpenApi.withSchema(
          "Pet",
          Fixed.sum(List(Fixed.reference("#/components/schemas/Dog"), Fixed.reference("#/components/schemas/Cat")))
        )
      ) must ===(s"""|object models {
            |$modelImports
            |import Pet._
            |type Pet = Dog :+: Cat :+: CNil
            |object Pet {
            |
            |  import org.http4s.{EntityEncoder, EntityDecoder}
            |  import org.http4s.circe._
            |  import cats.Applicative
            |  import cats.effect.Sync
            |  import cats._
            |  import cats.syntax.all._
            |  import io.circe._
            |  implicit val PetEncoder: Encoder[Pet] = Encoder.instance { x =>
            |import shapeless.Poly1
            |object json extends Poly1 {
            |
            |implicit def dog = at[Dog](x => Encoder[Dog].apply(x))
            |implicit def cat = at[Cat](x => Encoder[Cat].apply(x))
            |}
            |x.fold(json)
            |}
            |  implicit val PetDecoder: Decoder[Pet] = Decoder[Dog].map(x => Coproduct[Pet](x)) orElse Decoder[Cat].map(x => Coproduct[Pet](x))
            |  implicit def PetEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Pet] = jsonEncoderOf[F, Pet]
            |  implicit def OptionPetEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[Pet]] = jsonEncoderOf[F, Option[Pet]]
            |  implicit def PetEntityDecoder[F[_]:Sync]: EntityDecoder[F, Pet] = jsonOf[F, Pet]
            |
            |}
            |}""".stripMargin)

    }

    "when a sum type is provided with invalid identifier names" >> {

      import client.http4s.circe._
      model.print(
        petstoreOpenApi.withSchema(
          "1-Pet",
          Fixed.sum(List(Fixed.reference("#/components/schemas/2-Dog"), Fixed.reference("#/components/schemas/3-Cat")))
        )
      ) must ===(s"""|object models {
            |$modelImports
            |import `1-Pet`._
            |type `1-Pet` = `2-Dog` :+: `3-Cat` :+: CNil
            |object `1-Pet` {
            |
            |  import org.http4s.{EntityEncoder, EntityDecoder}
            |  import org.http4s.circe._
            |  import cats.Applicative
            |  import cats.effect.Sync
            |  import cats._
            |  import cats.syntax.all._
            |  import io.circe._
            |  implicit val Pet1Encoder: Encoder[`1-Pet`] = Encoder.instance { x =>
            |import shapeless.Poly1
            |object json extends Poly1 {
            |
            |implicit def `2-dog` = at[`2-Dog`](x => Encoder[`2-Dog`].apply(x))
            |implicit def `3-cat` = at[`3-Cat`](x => Encoder[`3-Cat`].apply(x))
            |}
            |x.fold(json)
            |}
            |  implicit val Pet1Decoder: Decoder[`1-Pet`] = Decoder[`2-Dog`].map(x => Coproduct[`1-Pet`](x)) orElse Decoder[`3-Cat`].map(x => Coproduct[`1-Pet`](x))
            |  implicit def Pet1EntityEncoder[F[_]:Applicative]: EntityEncoder[F, `1-Pet`] = jsonEncoderOf[F, `1-Pet`]
            |  implicit def OptionPet1EntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[`1-Pet`]] = jsonEncoderOf[F, Option[`1-Pet`]]
            |  implicit def Pet1EntityDecoder[F[_]:Sync]: EntityDecoder[F, `1-Pet`] = jsonOf[F, `1-Pet`]
            |
            |}
            |}""".stripMargin)

    }

    "when multiple types are provided" >> {
      import Printer.avoid._
      model.print(
        petstoreOpenApi
          .withSchema("Bar", obj("foo" -> Fixed.string())("foo"))
          .withSchema(
            "Bars",
            Fixed.array(Fixed.reference("#/components/schemas/Bar"))
          )
      ) must ===(s"""|object models {
                          |$modelImports
                          |final case class Bar(foo: String)
                          |object Bar {
                          |
                          |
                          |}
                          |type Bars = List[Bar]
                          |}""".stripMargin)
    }
  }

  "Client trait should able to print" >> {
    import client.print._
    "when there are no paths" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi) must ===("")
    }

    "when a post operation is provided" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi.withPath(mediaTypeReferencePost)) must ===( //
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def createPayload(newPayload: NewPayload): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |
           |}""".stripMargin
      )
    }

    "when a put and delete are provided" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi.withPath(mediaTypeReferencePutDelete)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayload(id: String): F[Unit]
           |  def updatePayload(id: String, updatePayload: UpdatePayload): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |
           |
           |
           |}""".stripMargin
      )
    }

    "when get endpoints are provided" >> {
      import client.http4s.circe._
      interfaceDefinition.print(
        payloadOpenApi.withPath(mediaTypeReferenceGet).withPath(mediaTypeReferenceGetId)
      ) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(limit: Option[Int], name: Option[String]): F[Payloads]
           |  def getPayload(id: String): F[Payload]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |
           |
           |
           |}""".stripMargin
      )
    }

    "when optional body and not optional query parameters are provided" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi.withPath(mediaTypeOptionBody)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayload(id: String, size: Long, updatePayload: Option[UpdatePayload]): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |
           |}""".stripMargin
      )
    }

    "when parameter is provided as enum" >> {
      import client.http4s.circe._
      interfaceDefinition.print(
        payloadOpenApi.withPath(enumParameterGet)
      ) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(status: parameters.status): F[Unit]
           |}
           |object PayloadClient {
           |
           |
           |
           |object parameters {
           |
           |sealed trait status
           |object status {
           |
           |  final case object Pending extends status
           |  final case object Active extends status
           |  import org.http4s.{EntityEncoder, EntityDecoder}
           |  import org.http4s.circe._
           |  import cats.Applicative
           |  import cats.effect.Sync
           |  import cats._
           |  import cats.syntax.all._
           |  import io.circe._
           |  implicit val StatusShow: Show[status] = Show.show {
           |  case Pending => "Pending"
           |  case Active => "Active"
           |}
           |  implicit val StatusEncoder: Encoder[status] = Encoder.encodeString.contramap(_.show)
           |  implicit val StatusDecoder: Decoder[status] = Decoder.decodeString.emap {
           |  case "Pending" => Pending.asRight
           |  case "Active" => Active.asRight
           |  case x => s"$x is not valid status".asLeft
           |}
           |
           |  implicit def StatusEntityEncoder[F[_]:Applicative]: EntityEncoder[F, status] = jsonEncoderOf[F, status]
           |  implicit def OptionStatusEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[status]] = jsonEncoderOf[F, Option[status]]
           |  implicit def StatusEntityDecoder[F[_]:Sync]: EntityDecoder[F, status] = jsonOf[F, status]
           |
           |}
           |}
           |
           |}""".stripMargin
      )

    }

    "when references in the request and the responses" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi.withPath(references)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(updatePayload: UpdatePayload): F[UpdatedPayload]
           |}
           |object PayloadClient {
           |
           |
           |
           |
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with a default one" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponsesWithDefaultOne)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(id: String): F[Either[GetPayloadErrorResponse, Payload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class GetPayloadUnexpectedErrorResponse(statusCode: Int, value: Error)
           |  type GetPayloadErrorResponse = GetPayloadUnexpectedErrorResponse
           |
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with not found response" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi.withPath(notFoundResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def getPayload(id: String): F[Either[GetPayloadErrorResponse, Payload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class GetPayloadNotFoundError(value: String)
           |  type GetPayloadErrorResponse = GetPayloadNotFoundError
           |
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with anonymous objects" >> {
      import client.http4s.circe._
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObject)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(id: String): F[Either[UpdatePayloadErrorResponse, UpdatedPayload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class UpdatedPayload(name: String)
           |object UpdatedPayload {
           |
           |  import io.circe._
           |  import io.circe.generic.semiauto._
           |  import org.http4s.{EntityEncoder, EntityDecoder}
           |  import org.http4s.circe._
           |  import cats.Applicative
           |  import cats.effect.Sync
           |  implicit val UpdatedPayloadEncoder: Encoder[UpdatedPayload] = deriveEncoder[UpdatedPayload]
           |  implicit val UpdatedPayloadDecoder: Decoder[UpdatedPayload] = deriveDecoder[UpdatedPayload]
           |  implicit def UpdatedPayloadEntityEncoder[F[_]:Applicative]: EntityEncoder[F, UpdatedPayload] = jsonEncoderOf[F, UpdatedPayload]
           |  implicit def OptionUpdatedPayloadEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[UpdatedPayload]] = jsonEncoderOf[F, Option[UpdatedPayload]]
           |  implicit def UpdatedPayloadEntityDecoder[F[_]:Sync]: EntityDecoder[F, UpdatedPayload] = jsonOf[F, UpdatedPayload]
           |
           |}
           |  final case class UpdatePayloadNotFound(isDone: Boolean)
           |object UpdatePayloadNotFound {
           |
           |  import io.circe._
           |  import io.circe.generic.semiauto._
           |  import org.http4s.{EntityEncoder, EntityDecoder}
           |  import org.http4s.circe._
           |  import cats.Applicative
           |  import cats.effect.Sync
           |  implicit val UpdatePayloadNotFoundEncoder: Encoder[UpdatePayloadNotFound] = deriveEncoder[UpdatePayloadNotFound]
           |  implicit val UpdatePayloadNotFoundDecoder: Decoder[UpdatePayloadNotFound] = deriveDecoder[UpdatePayloadNotFound]
           |  implicit def UpdatePayloadNotFoundEntityEncoder[F[_]:Applicative]: EntityEncoder[F, UpdatePayloadNotFound] = jsonEncoderOf[F, UpdatePayloadNotFound]
           |  implicit def OptionUpdatePayloadNotFoundEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[UpdatePayloadNotFound]] = jsonEncoderOf[F, Option[UpdatePayloadNotFound]]
           |  implicit def UpdatePayloadNotFoundEntityDecoder[F[_]:Sync]: EntityDecoder[F, UpdatePayloadNotFound] = jsonOf[F, UpdatePayloadNotFound]
           |
           |}
           |  type UpdatePayloadErrorResponse = UpdatePayloadNotFound
           |
           |
           |}""".stripMargin
      )
    }

    "when there are simple response and response with anonymous objects" >> {
      import client.http4s.circe._
      interfaceDefinition.print(anotherPayloadOpenApi.withPath(objectRequestResponseAnonymousObjects)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait AnotherPayloadClient[F[_]] {
           |  import AnotherPayloadClient._
           |  def updateAnotherPayload(id: String, updateAnotherPayloadRequest: UpdateAnotherPayloadRequest): F[UpdatedPayload]
           |}
           |object AnotherPayloadClient {
           |
           |  type UpdateAnotherPayloadRequest = io.circe.Json
           |  type UpdatedPayload = io.circe.Json
           |
           |
           |}""".stripMargin
      )
    }

    "when there are simple response and response with anonymous objects" >> {
      import client.http4s.circe._
      interfaceDefinition.print(anotherPayloadOpenApi.withPath(simpleRequestResponseAnonymousObjects)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait AnotherPayloadClient[F[_]] {
           |  import AnotherPayloadClient._
           |  def updateAnotherPayload(id: String, updateAnotherPayloadRequest: UpdateAnotherPayloadRequest): F[UpdatedPayload]
           |}
           |object AnotherPayloadClient {
           |
           |  final case class UpdateAnotherPayloadRequest(name: String)
           |object UpdateAnotherPayloadRequest {
           |
           |  import io.circe._
           |  import io.circe.generic.semiauto._
           |  import org.http4s.{EntityEncoder, EntityDecoder}
           |  import org.http4s.circe._
           |  import cats.Applicative
           |  import cats.effect.Sync
           |  implicit val UpdateAnotherPayloadRequestEncoder: Encoder[UpdateAnotherPayloadRequest] = deriveEncoder[UpdateAnotherPayloadRequest]
           |  implicit val UpdateAnotherPayloadRequestDecoder: Decoder[UpdateAnotherPayloadRequest] = deriveDecoder[UpdateAnotherPayloadRequest]
           |  implicit def UpdateAnotherPayloadRequestEntityEncoder[F[_]:Applicative]: EntityEncoder[F, UpdateAnotherPayloadRequest] = jsonEncoderOf[F, UpdateAnotherPayloadRequest]
           |  implicit def OptionUpdateAnotherPayloadRequestEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[UpdateAnotherPayloadRequest]] = jsonEncoderOf[F, Option[UpdateAnotherPayloadRequest]]
           |  implicit def UpdateAnotherPayloadRequestEntityDecoder[F[_]:Sync]: EntityDecoder[F, UpdateAnotherPayloadRequest] = jsonOf[F, UpdateAnotherPayloadRequest]
           |
           |}
           |  final case class UpdatedPayload(name: String)
           |object UpdatedPayload {
           |
           |  import io.circe._
           |  import io.circe.generic.semiauto._
           |  import org.http4s.{EntityEncoder, EntityDecoder}
           |  import org.http4s.circe._
           |  import cats.Applicative
           |  import cats.effect.Sync
           |  implicit val UpdatedPayloadEncoder: Encoder[UpdatedPayload] = deriveEncoder[UpdatedPayload]
           |  implicit val UpdatedPayloadDecoder: Decoder[UpdatedPayload] = deriveDecoder[UpdatedPayload]
           |  implicit def UpdatedPayloadEntityEncoder[F[_]:Applicative]: EntityEncoder[F, UpdatedPayload] = jsonEncoderOf[F, UpdatedPayload]
           |  implicit def OptionUpdatedPayloadEntityEncoder[F[_]:Applicative]: EntityEncoder[F, Option[UpdatedPayload]] = jsonEncoderOf[F, Option[UpdatedPayload]]
           |  implicit def UpdatedPayloadEntityDecoder[F[_]:Sync]: EntityDecoder[F, UpdatedPayload] = jsonOf[F, UpdatedPayload]
           |
           |}
           |
           |
           |}""".stripMargin
      )

    }

    "when multiple responses with anonymous objects with default response" >> {
      import Printer.avoid._
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObjectAndDefaultOne)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def updatePayload(id: String): F[Either[UpdatePayloadErrorResponse, UpdatedPayload]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class UpdatedPayload(name: String)
           |object UpdatedPayload {
           |
           |
           |}
           |  final case class UpdatePayloadUnexpectedError(isDone: Boolean)
           |object UpdatePayloadUnexpectedError {
           |
           |
           |}
           |  final case class UpdatePayloadUnexpectedErrorResponse(statusCode: Int, value: UpdatePayloadUnexpectedError)
           |  type UpdatePayloadErrorResponse = UpdatePayloadUnexpectedErrorResponse
           |
           |
           |}""".stripMargin
      )
    }

    "when multiple responses and multiple error scenarios" >> {
      import Printer.avoid._
      interfaceDefinition.print(payloadOpenApi.withPath(multipleResponses)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def createPayload(): F[Either[CreatePayloadErrorResponse, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class CreatePayloadNotFoundError(value: String)
           |  final case class CreatePayloadUnexpectedErrorResponse(statusCode: Int, value: Error)
           |  type CreatePayloadErrorResponse = CreatePayloadNotFoundError :+: CreatePayloadUnexpectedErrorResponse :+: CNil
           |
           |
           |}""".stripMargin
      )
    }

    "two operations with default response" >> {
      import Printer.avoid._
      interfaceDefinition.print(payloadOpenApi.withPath(twoOperationsWithDefaultResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def createPayloads(): F[Either[CreatePayloadsErrorResponse, Unit]]
           |  def updatePayloads(): F[Either[UpdatePayloadsErrorResponse, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |
           |  final case class CreatePayloadsUnexpectedError(name: String)
           |object CreatePayloadsUnexpectedError {
           |
           |
           |}
           |  final case class CreatePayloadsUnexpectedErrorResponse(statusCode: Int, value: CreatePayloadsUnexpectedError)
           |  type CreatePayloadsErrorResponse = CreatePayloadsUnexpectedErrorResponse
           |  final case class UpdatePayloadsUnexpectedError(isDone: Boolean)
           |object UpdatePayloadsUnexpectedError {
           |
           |
           |}
           |  final case class UpdatePayloadsUnexpectedErrorResponse(statusCode: Int, value: UpdatePayloadsUnexpectedError)
           |  type UpdatePayloadsErrorResponse = UpdatePayloadsUnexpectedErrorResponse
           |
           |
           |}""".stripMargin
      )
    }

    "when the failure response is empty" >> {
      import Printer.avoid._
      interfaceDefinition.print(payloadOpenApi.withPath(emptyErrorResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayloads(): F[Either[DeletePayloadsErrorResponse, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class DeletePayloadsNotFoundError(value: Unit)
           |  type DeletePayloadsErrorResponse = DeletePayloadsNotFoundError
           |
           |
           |}""".stripMargin
      )
    }

    "when multiple failure response are empty" >> {
      import Printer.avoid._
      interfaceDefinition.print(payloadOpenApi.withPath(multipleEmptyErrorResponse)) must ===(
        """|import models._
           |import shapeless.{:+:, CNil}
           |trait PayloadClient[F[_]] {
           |  import PayloadClient._
           |  def deletePayloads(): F[Either[DeletePayloadsErrorResponse, Unit]]
           |}
           |object PayloadClient {
           |
           |
           |  final case class DeletePayloadsNotFoundError(value: Unit)
           |  final case class DeletePayloadsUnexpectedErrorResponse(statusCode: Int, value: Unit)
           |  type DeletePayloadsErrorResponse = DeletePayloadsNotFoundError :+: DeletePayloadsUnexpectedErrorResponse :+: CNil
           |
           |
           |}""".stripMargin
      )
    }

    "when a post operation is provided and operation id is not provided" >> {
      import Printer.avoid._
      interfaceDefinition.print(
        petstoreOpenApi
          .withPath(
            "/pets" -> emptyItemObject.withPost(
              operation[JsonSchemaF.Fixed](
                request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPet"))),
                responses = "200"          -> response("Null response")
              )
            )
          )
          .withPath(
            "/pets/{id}" -> emptyItemObject.withPut(
              operation[JsonSchemaF.Fixed](
                request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePet"))),
                responses = "201"          -> response("Null response")
              ).withParameter(path("id", Fixed.string()))
            )
          )
          .withPath(
            "/pets/{id}/owners/" -> emptyItemObject.withGet(
              operation[JsonSchemaF.Fixed](
                request(),
                responses = "201" -> response(
                  "Null response",
                  "application/json" -> mediaType(Fixed.reference("#/components/schemas/Owners"))
                )
              ).withParameter(path("id", Fixed.string()))
            )
          )
      ) must ===("""|import models._
                    |import shapeless.{:+:, CNil}
                    |trait PetstoreClient[F[_]] {
                    |  import PetstoreClient._
                    |  def createPets(newPet: NewPet): F[Unit]
                    |  def updatePetsById(id: String, updatePet: UpdatePet): F[Unit]
                    |  def getOwnersPetsById(id: String): F[Owners]
                    |}
                    |object PetstoreClient {
                    |
                    |
                    |
                    |
                    |
                    |
                    |
                    |
                    |
                    |}""".stripMargin)
    }

  }

  "share http4s impl should able to print" >> {
    import client.print._
    import client.http4s.print.implDefinition
    import client.http4s.print.Http4sSpecifics
    import Printer.avoid._
    implicit val none: Http4sSpecifics = new Http4sSpecifics {
      def none[A]                                            = Printer.unit.contramap[A](_ => ())
      def applyMethod: Printer[(TraitName, ImplName)]        = none
      def withBody: Printer[Var]                             = Printer(x => s".with(${x.show})")
      def withHeaders[T]: Printer[List[Parameter.Header[T]]] = none

    }

    "when there are no paths" >> {
      implDefinition.print(petstoreOpenApi) must ===("")
    }

    "when a put and delete are provided" >> {
      implDefinition.print(petstoreOpenApi.withPath(mediaTypeReferencePutDelete)) must ===(
        s"""|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |$defaultImplicits
           |    def deletePayload(id: String): F[Unit] = client.expect[Unit](Request[F](method = Method.DELETE, uri = baseUrl / "payloads" / id.show))
           |    def updatePayload(id: String, updatePayload: UpdatePayload): F[Unit] = client.expect[Unit](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show).with(updatePayload))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when get endpoints are provided" >> {
      implDefinition.print(petstoreOpenApi.withPath(mediaTypeReferenceGet).withPath(mediaTypeReferenceGetId)) must ===(
        s"""|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |$defaultImplicits
           |    def getPayload(limit: Option[Int], name: Option[String]): F[Payloads] = client.expect[Payloads](Request[F](method = Method.GET, uri = baseUrl / "payloads" +?? ("limit", limit) +?? ("name", name)))
           |    def getPayload(id: String): F[Payload] = client.expect[Payload](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when parameters are provided as references" >> {
      implDefinition.print(
        petstoreOpenApi
          .withPath(parametersReferenceGet)
          .withParameter("initParam", query("init", JsonSchemaF.Fixed.long(), required = true))
          .withParameter("limitParam", query("limit", JsonSchemaF.Fixed.integer()))
      ) must ===(
        s"""|object PetstoreHttpClient {
          |
          |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
          |    import PetstoreClient._
          |$defaultImplicits
          |    def getPayload(id: String, init: Long, limit: Option[Int], color: Color): F[Payloads] = client.expect[Payloads](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show +? ("init", init) +?? ("limit", limit)))
          |  }
          |
          |}""".stripMargin
      )
    }

    "when optional body and not optional query parameters is provided" >> {
      implDefinition.print(petstoreOpenApi.withPath(mediaTypeOptionBody)) must ===(
        s"""|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |$defaultImplicits
           |    def deletePayload(id: String, size: Long, updatePayload: Option[UpdatePayload]): F[Unit] = client.expect[Unit](Request[F](method = Method.DELETE, uri = baseUrl / "payloads" / id.show +? ("size", size)).with(updatePayload))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when references in the request and the responses" >> {
      implDefinition.print(petstoreOpenApi.withPath(references)) must ===(
        s"""|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |$defaultImplicits
           |    def updatePayload(updatePayload: UpdatePayload): F[UpdatedPayload] = client.expect[UpdatedPayload](Request[F](method = Method.PUT, uri = baseUrl / "payloads").with(updatePayload))
           |  }
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with a default one" >> {
      implDefinition.print(petstoreOpenApi.withPath(multipleResponsesWithDefaultOne)) must ===(
        s"""|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |$defaultImplicits
           |    def getPayload(id: String): F[Either[GetPayloadErrorResponse, Payload]] = client.fetch[Either[GetPayloadErrorResponse, Payload]](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[Payload].map(_.asRight)
           |      case default => default.as[Error].map(x => GetPayloadUnexpectedErrorResponse(default.status.code, x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with not found response" >> {
      implDefinition.print(petstoreOpenApi.withPath(notFoundResponse)) must ===(s"""|object PetstoreHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
           |    import PetstoreClient._
           |$defaultImplicits
           |    def getPayload(id: String): F[Either[GetPayloadErrorResponse, Payload]] = client.fetch[Either[GetPayloadErrorResponse, Payload]](Request[F](method = Method.GET, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[Payload].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[String].map(x => GetPayloadNotFoundError(x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin)
    }

    "when there are simple response and response with anonymous objects" >> {
      implDefinition.print(petstoreOpenApi.withPath(simpleRequestResponseAnonymousObjects)) must ===(
        s"""|object PetstoreHttpClient {
          |
          |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
          |    import PetstoreClient._
          |$defaultImplicits
          |    def updateAnotherPayload(id: String, updateAnotherPayloadRequest: UpdateAnotherPayloadRequest): F[UpdatedPayload] = client.expect[UpdatedPayload](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show).with(updateAnotherPayloadRequest))
          |  }
          |
          |}""".stripMargin
      )
    }

    "when multiple responses with anonymous objects with default response" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObjectAndDefaultOne)) must ===(
        s"""|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |$defaultImplicits
           |    def updatePayload(id: String): F[Either[UpdatePayloadErrorResponse, UpdatedPayload]] = client.fetch[Either[UpdatePayloadErrorResponse, UpdatedPayload]](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[UpdatedPayload].map(_.asRight)
           |      case default => default.as[UpdatePayloadUnexpectedError].map(x => UpdatePayloadUnexpectedErrorResponse(default.status.code, x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when multiple responses and multiple error scenarios" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleResponses)) must ===(
        s"""|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |$defaultImplicits
           |    def createPayload(): F[Either[CreatePayloadErrorResponse, Unit]] = client.fetch[Either[CreatePayloadErrorResponse, Unit]](Request[F](method = Method.POST, uri = baseUrl / "payloads")) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[String].map(x => Coproduct[CreatePayloadErrorResponse](CreatePayloadNotFoundError(x)).asLeft)
           |      case default => default.as[Error].map(x => Coproduct[CreatePayloadErrorResponse](CreatePayloadUnexpectedErrorResponse(default.status.code, x)).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when there are multiple responses with anonymous objects" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleResponsesWithAnonymousObject)) must ===(
        s"""|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |$defaultImplicits
           |    def updatePayload(id: String): F[Either[UpdatePayloadErrorResponse, UpdatedPayload]] = client.fetch[Either[UpdatePayloadErrorResponse, UpdatedPayload]](Request[F](method = Method.PUT, uri = baseUrl / "payloads" / id.show)) {
           |      case Successful(response) => response.as[UpdatedPayload].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[UpdatePayloadNotFound].map(x => x.asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when the failure response is empty" >> {
      implDefinition.print(payloadOpenApi.withPath(emptyErrorResponse)) must ===(
        s"""|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |$defaultImplicits
           |    def deletePayloads(): F[Either[DeletePayloadsErrorResponse, Unit]] = client.fetch[Either[DeletePayloadsErrorResponse, Unit]](Request[F](method = Method.DELETE, uri = baseUrl / "payloads")) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[Unit].map(x => DeletePayloadsNotFoundError(x).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when multiple failure response are empty" >> {
      implDefinition.print(payloadOpenApi.withPath(multipleEmptyErrorResponse)) must ===(
        s"""|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |$defaultImplicits
           |    def deletePayloads(): F[Either[DeletePayloadsErrorResponse, Unit]] = client.fetch[Either[DeletePayloadsErrorResponse, Unit]](Request[F](method = Method.DELETE, uri = baseUrl / "payloads")) {
           |      case Successful(response) => response.as[Unit].map(_.asRight)
           |      case response if response.status.code == 404 => response.as[Unit].map(x => Coproduct[DeletePayloadsErrorResponse](DeletePayloadsNotFoundError(x)).asLeft)
           |      case default => default.as[Unit].map(x => Coproduct[DeletePayloadsErrorResponse](DeletePayloadsUnexpectedErrorResponse(default.status.code, x)).asLeft)
           |    }
           |  }
           |
           |}""".stripMargin
      )
    }

    "when params are invalid identifier names" >> {

      implDefinition.print(payloadOpenApi.withPath(notNormalizeRequest)) must ===(
        s"""|object PayloadHttpClient {
           |
           |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PayloadClient[F] = new PayloadClient[F] {
           |    import PayloadClient._
           |$defaultImplicits
           |    def getId1ById1(`1id`: String, `limit-for`: Option[Int], `List[String]`: List[String]): F[Unit] = client.expect[Unit](Request[F](method = Method.GET, uri = baseUrl / "1id" / `1id`.show +?? ("limit-for", `limit-for`)).with(`List[String]`))
           |  }
           |
           |}""".stripMargin
      )
    }
  }

  "http4s 0.20.x should able to print" >> {
    import client.http4s.print.impl
    import client.http4s.print.v20._
    import Printer.avoid._

    "when a post operation is provided" >> {
      impl.print(
        PackageName("petstore") -> petstoreOpenApi
          .withPath(mediaTypeReferences)
          .withSchema("NewPayloads", Fixed.array(Fixed.reference("NewPayload")))
          .withSchema("FooBar", Fixed.sum(List(Fixed.reference("Foo"), Fixed.reference("Bar"))))
      ) must ===(
        s"""|import cats._
            |import cats.effect._
            |import cats.syntax.all._
            |import io.circe._
            |import org.http4s._
            |import org.http4s.client.Client
            |import org.http4s.client.blaze._
            |import org.http4s.circe._
            |import org.http4s.Status.Successful
            |import shapeless.Coproduct
            |import scala.concurrent.ExecutionContext
            |import petstore.PetstoreClient
            |import petstore.models._
            |import FooBar._
            |object PetstoreHttpClient {
            |
            |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
            |    import PetstoreClient._
            |$defaultImplicits
            |    def createPayloads(`X-Auth`: String, token: String, newPayloads: NewPayloads): F[Either[CreatePayloadsErrorResponse, Unit]] = client.fetch[Either[CreatePayloadsErrorResponse, Unit]](Request[F](method = Method.POST, uri = baseUrl / "payloads").withEntity(newPayloads).withHeaders(Headers.of(Header("X-Auth", `X-Auth`.show), Header("token", token.show)))) {
            |      case Successful(response) => response.as[Unit].map(_.asRight)
            |      case default => default.as[Error].map(x => CreatePayloadsUnexpectedErrorResponse(default.status.code, x).asLeft)
            |    }
            |    def updatePayloads(payloads: Payloads): F[Payloads] = client.expect[Payloads](Request[F](method = Method.PUT, uri = baseUrl / "payloads").withEntity(payloads))
            |  }
            |  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext, $timeQueryParamEncoders): Resource[F, PetstoreClient[F]] = BlazeClientBuilder(executionContext).resource.map(PetstoreHttpClient.build(_, baseUrl))
            |}""".stripMargin
      )
    }
  }

  "http4s 0.18.x should able to print" >> {
    import client.http4s.print.impl
    import client.http4s.print.v18._
    import Printer.avoid._

    "when a post operation is provided" >> {
      impl.print(PackageName("petstore") -> petstoreOpenApi.withPath(mediaTypeReferences)) must ===(
        s"""|import cats._
        |import cats.effect._
        |import cats.syntax.all._
        |import io.circe._
        |import org.http4s._
        |import org.http4s.client.Client
        |import org.http4s.client.blaze._
        |import org.http4s.circe._
        |import org.http4s.Status.Successful
        |import shapeless.Coproduct
        |import scala.concurrent.ExecutionContext
        |import petstore.PetstoreClient
        |import petstore.models._
        |object PetstoreHttpClient {
        |
        |  def build[F[_]: Effect: Sync](client: Client[F], baseUrl: Uri)($timeQueryParamEncodersImplicit): PetstoreClient[F] = new PetstoreClient[F] {
        |    import PetstoreClient._
        |$defaultImplicits
        |    def createPayloads(`X-Auth`: String, token: String, newPayloads: NewPayloads): F[Either[CreatePayloadsErrorResponse, Unit]] = client.fetch[Either[CreatePayloadsErrorResponse, Unit]](Request[F](method = Method.POST, uri = baseUrl / "payloads").withBody(newPayloads).withHeaders(Headers(Header("X-Auth", `X-Auth`.show), Header("token", token.show)))) {
        |      case Successful(response) => response.as[Unit].map(_.asRight)
        |      case default => default.as[Error].map(x => CreatePayloadsUnexpectedErrorResponse(default.status.code, x).asLeft)
        |    }
        |    def updatePayloads(payloads: Payloads): F[Payloads] = client.expect[Payloads](Request[F](method = Method.PUT, uri = baseUrl / "payloads").withBody(payloads))
        |  }
        |  def apply[F[_]: ConcurrentEffect](baseUrl: Uri)(implicit executionContext: ExecutionContext, $timeQueryParamEncoders): F[PetstoreClient[F]] = Http1Client[F](config = BlazeClientConfig.defaultConfig.copy(executionContext = executionContext)).map(PetstoreHttpClient.build(_, baseUrl))
        |}""".stripMargin
      )
    }

    "when there are no paths" >> {
      import client.http4s.print.impl
      import client.http4s.print.v18._
      import Printer.avoid._
      impl.print(PackageName("petstore") -> petstoreOpenApi) must ===("")
    }

  }
}

object OpenApiPrintSpecification {
  import JsonSchemaF.Fixed
  import helpers._

  private val pathId        = path("id", Fixed.string())
  private val payloadPath   = "/payloads"
  private val payloadPathId = s"$payloadPath/{id}"

  def petstoreOpenApi[T]       = openApi[T]("Petstore")
  def payloadOpenApi[T]        = openApi[T]("Payload")
  def anotherPayloadOpenApi[T] = openApi[T]("AnotherPayload")

  private val successPayload = "200" -> response(
    "Null response",
    "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payload"))
  )

  private val successNull = "200" -> response[JsonSchemaF.Fixed]("Null response")

  private val defaultError = "default" -> response(
    "Unexpected error",
    "application/json" -> mediaType(Fixed.reference("#/components/schemas/Error"))
  )

  private val notFound = "404" -> response(
    "Not found",
    "application/json" -> mediaType(Fixed.string())
  )

  val mediaTypeReferencePost = "/payloads" -> emptyItemObject.withPost(
    operation[JsonSchemaF.Fixed](
      request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPayload"))),
      responses = "201"          -> response("Null response")
    ).withOperationId("createPayload")
  )

  val mediaTypeReferences = "/payloads" -> emptyItemObject
    .withPost(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/NewPayloads"))),
        responses = "201"          -> response(""),
        defaultError
      ).withOperationId("createPayloads")
        .withParameter(header("X-Auth", Fixed.string()))
        .withParameter(header("token", Fixed.string()))
    )
    .withPut(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/Payloads"))),
        responses = "200" -> response(
          "",
          "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payloads"))
        )
      ).withOperationId("updatePayloads")
    )

  val mediaTypeReferencePutDelete = payloadPathId -> emptyItemObject
    .withPut(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePayload"))),
        successNull
      ).withOperationId("updatePayload").withParameter(pathId)
    )
    .withDelete(
      operation[JsonSchemaF.Fixed](
        request(),
        successNull
      ).withOperationId("deletePayload").withParameter(pathId)
    )

  val mediaTypeReferenceGet = payloadPath -> emptyItemObject.withGet(
    operationWithResponses[JsonSchemaF.Fixed](
      responses = "200" -> response(
        "",
        "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payloads"))
      )
    ).withOperationId("getPayload")
      .withParameter(query("limit", Fixed.integer()))
      .withParameter(query("name", Fixed.string()))
  )

  val parametersReferenceGet = payloadPathId -> emptyItemObject
    .withParameter(path("id", JsonSchemaF.Fixed.string()))
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = "200" -> response(
          "",
          "application/json" -> mediaType(Fixed.reference("#/components/schemas/Payloads"))
        )
      ).withOperationId("getPayload")
        .withParameter(path("id", JsonSchemaF.Fixed.string()))
        .withParameter(Reference("#/components/parameters/initParam"))
        .withParameter(Reference("#/components/parameters/limitParam"))
        .withParameter(path("color", JsonSchemaF.Fixed.reference("#/components/schemas/Color")))
    )

  val enumParameterGet = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successNull
      ).withOperationId("getPayload")
    )
    .withParameter(query("status", Fixed.enum(List("Pending", "Active")), required = true))

  val mediaTypeReferenceGetId = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](successPayload)
        .withOperationId("getPayload")
        .withParameter(path("id", Fixed.string()))
    )

  val mediaTypeOptionBody = payloadPathId -> emptyItemObject
    .withDelete(
      operation[JsonSchemaF.Fixed](
        request("application/json" -> mediaType(Fixed.reference("#/components/schemas/UpdatePayload"))).optional,
        successNull
      ).withOperationId("deletePayload")
        .withParameter(path("id", Fixed.string()))
        .withParameter(query("size", Fixed.long(), required = true))
    )

  val references = payloadPath -> emptyItemObject
    .withPut(
      operationWithReferences[JsonSchemaF.Fixed](
        reference("#/components/schemas/UpdatePayload"),
        responses = "200" -> reference("#/components/schemas/UpdatedPayload")
      ).withOperationId("updatePayload")
    )

  val multipleResponsesWithDefaultOne = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successPayload,
        defaultError
      ).withOperationId("getPayload").withParameter(path("id", Fixed.string()))
    )

  val notFoundResponse = payloadPathId -> emptyItemObject
    .withGet(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successPayload,
        notFound
      ).withOperationId("getPayload").withParameter(path("id", Fixed.string()))
    )

  val multipleResponsesWithAnonymousObject = payloadPathId -> emptyItemObject
    .withPut(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = "200" -> response(
          "Updated payload",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        ),
        "404" -> response(
          "Not found",
          "application/json" -> mediaType(obj("isDone" -> Fixed.boolean())("isDone"))
        )
      ).withOperationId("updatePayload").withParameter(path("id", Fixed.string()))
    )

  val objectRequestResponseAnonymousObjects = "/payloads/{id}" -> emptyItemObject
    .withPut(
      operation[JsonSchemaF.Fixed](
        request(
          "*/*" -> mediaType(obj()())
        ),
        responses = "200" -> response(
          "Updated payload",
          "application/json" -> mediaType(obj()())
        )
      ).withOperationId("updateAnotherPayload").withParameter(path("id", Fixed.string()))
    )

  val simpleRequestResponseAnonymousObjects = "/payloads/{id}" -> emptyItemObject
    .withPut(
      operation[JsonSchemaF.Fixed](
        request(
          "*/*" -> mediaType(obj("name" -> Fixed.string())("name"))
        ),
        responses = "200" -> response(
          "Updated payload",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        )
      ).withOperationId("updateAnotherPayload").withParameter(path("id", Fixed.string()))
    )

  val multipleResponsesWithAnonymousObjectAndDefaultOne = "/payloads/{id}" -> emptyItemObject
    .withPut(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = "200" -> response(
          "Updated payload",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        ),
        "default" -> response(
          "Unexpected error",
          "application/json" -> mediaType(obj("isDone" -> Fixed.boolean())("isDone"))
        )
      ).withOperationId("updatePayload").withParameter(path("id", Fixed.string()))
    )

  val multipleResponses = "/payloads" -> emptyItemObject
    .withPost(
      operationWithResponses[JsonSchemaF.Fixed](successNull, notFound, defaultError).withOperationId("createPayload")
    )

  val twoOperationsWithDefaultResponse = "/payloads/" -> emptyItemObject
    .withPost(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successNull,
        "default" -> response(
          "Unexpected error",
          "application/json" -> mediaType(obj("name" -> Fixed.string())("name"))
        )
      )
    )
    .withPut(
      operationWithResponses[JsonSchemaF.Fixed](
        responses = successNull,
        "default" -> response(
          "Unexpected error",
          "*/*" -> mediaType(obj("isDone" -> Fixed.boolean())("isDone"))
        )
      )
    )

  val emptyErrorResponse = "/payloads/" -> emptyItemObject.withDelete(
    operationWithResponses[JsonSchemaF.Fixed](
      responses = successNull,
      "404" -> response("Not found")
    )
  )

  val multipleEmptyErrorResponse = "payloads" -> emptyItemObject.withDelete(
    operationWithResponses[JsonSchemaF.Fixed](
      responses = successNull,
      "404"     -> response("Not found"),
      "default" -> response("Unexpected error")
    )
  )

  val notNormalizeRequest = "/1id/{1id}" -> emptyItemObject
    .withGet(
      operation[JsonSchemaF.Fixed](request("application/json" -> mediaType(Fixed.array(Fixed.string()))), successNull)
        .withParameter(path("1id", Fixed.string()))
        .withParameter(query("limit-for", Fixed.integer()))
    )

  val modelImports =
    """|import shapeless.{:+:, CNil}
       |import shapeless.Coproduct""".stripMargin
  val defaultImplicits =
    """|    implicit def listEntityEncoder[T: Encoder]: EntityEncoder[F, List[T]] = jsonEncoderOf[F, List[T]]
       |    implicit def listEntityDecoder[T: Decoder]: EntityDecoder[F, List[T]] = jsonOf[F, List[T]]
       |    implicit def optionListEntityEncoder[T: Encoder]: EntityEncoder[F, Option[List[T]]] = jsonEncoderOf[F, Option[List[T]]]
       |    implicit def optionListEntityDecoder[T: Decoder]: EntityDecoder[F, Option[List[T]]] = jsonOf[F, Option[List[T]]]
       |    implicit def showQueryParameter[T: Show]: QueryParamEncoder[T] = QueryParamEncoder.stringQueryParamEncoder.contramap(_.show)""".stripMargin

  val timeQueryParamEncoders =
    "localDateTimeQueryEncoder: QueryParamEncoder[java.time.LocalDateTime], localDateQueryEncoder: QueryParamEncoder[java.time.LocalDate]"
  val timeQueryParamEncodersImplicit =
    s"implicit $timeQueryParamEncoders"

}
