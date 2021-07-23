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
import higherkindness.skeuomorph.openapi.print._
import client.http4s.print._
import higherkindness.droste._
import cats.syntax.all._

import higherkindness.skeuomorph.openapi.print.Codecs

package object circe {
  private val http4sPackages =
    List("org.http4s.{EntityEncoder, EntityDecoder}", "org.http4s.circe._", "cats.Applicative", "cats.effect.Sync").map(
      PackageName.apply
    )

  private val packages =
    List("io.circe._", "io.circe.generic.semiauto._").map(PackageName.apply) ++ http4sPackages

  private val enumPackages = http4sPackages ++
    List("cats._", "cats.implicits._", "io.circe._").map(PackageName.apply)

  private def codecsTypes[T](name: String): ((String, Tpe[T]), (String, Tpe[T])) = {
    val tpe = Tpe[T](name)
    (name -> tpe) -> (s"Option${normalize(name)}" -> tpe.copy(required = false))
  }
  protected def isIdentifier: String => Boolean =
    field => field.headOption.exists(x => x.isLetter && x.isLower) && field.forall(_.isLetterOrDigit)

  implicit def circeCodecsPrinter[T: Basis[JsonSchemaF, *]]: Printer[Codecs] =
    (
      sepBy(space *< space *< importDef, "\n") >* newLine,
      optional(space *< space *< showEnum >* newLine),
      space *< space *< (optional(circeEncoder[T]) >|< forProductCirceEncoder[
        T
      ] >|< enumCirceEncoder >|< sumCirceEncoder) >* newLine,
      space *< space *< (optional(circeDecoder[T]) >|< forProductCirceDecoder[
        T
      ] >|< enumCirceDecoder >|< sumCirceDecoder) >* newLine,
      space *< space *< entityEncoder[T] >* newLine,
      space *< space *< entityEncoder[T] >* newLine,
      space *< space *< entityDecoder[T] >* newLine
    ).contramapN {
      case CaseClassCodecs(name, fields) if fields.forall(isIdentifier) =>
        val (default, optionType) = codecsTypes[T](name)
        (
          packages,
          none,
          default.some.asLeft.asLeft.asLeft,
          default.some.asLeft.asLeft.asLeft,
          default,
          optionType,
          default
        )
      case CaseClassCodecs(name, fields) =>
        val (default, optionType) = codecsTypes[T](name)
        val withFields            = un(second(default)(x => x -> fields))
        (
          packages,
          none,
          withFields.asRight.asLeft.asLeft,
          withFields.asRight.asLeft.asLeft,
          default,
          optionType,
          default
        )
      case EnumCodecs(name, values) =>
        val (default, optionType) = codecsTypes[T](name)
        (
          enumPackages,
          (name -> name, values.map(x => ident(x) -> x)).some,
          name.asRight.asLeft,
          (name, values).asRight.asLeft,
          default,
          optionType,
          default
        )
      case SumCodecs(name, values) =>
        val (default, optionType) = codecsTypes[T](name)
        (enumPackages, none, (name, values).asRight, (name, values).asRight, default, optionType, default)

    }

  implicit def http4sCodecsPrinter[T: Basis[JsonSchemaF, *]]: Printer[EntityCodecs[T]] =
    (
      sepBy(space *< space *< importDef, "\n") >* newLine,
      space *< space *< entityEncoder[T] >* newLine,
      space *< space *< entityDecoder[T] >* newLine
    ).contramapN { x =>
      val y = x.name -> Tpe[T](x.name)
      (packages, y, y)
    }

  private def decoderDef[T: Basis[JsonSchemaF, *], B](body: Printer[B]): Printer[(String, Tpe[T], B)] =
    implicitVal(body).contramap { case (x, y, z) => (normalize(x), "Decoder", y, z) }

  private def encoderDef[T: Basis[JsonSchemaF, *], B](body: Printer[B]): Printer[(String, Tpe[T], B)] =
    implicitVal(body).contramap { case (x, y, z) => (normalize(x), "Encoder", y, z) }

  def enumCirceEncoder[T: Basis[JsonSchemaF, *], B]: Printer[String] =
    encoderDef(κ("Encoder.encodeString.contramap(_.show)")).contramap(x => (x, Tpe[T](x), ()))

  def enumCirceDecoder[T: Basis[JsonSchemaF, *], B]: Printer[(String, List[String])] =
    decoderDef[T, (String, List[String])](
      (
        κ("Decoder.decodeString.emap {") *< newLine *<
          sepBy[(String, String)](
            (space *< space *< κ("case \"") *< string >* κ("\" => "), string >* κ(".asRight"))
              .contramapN(identity),
            "\n"
          ) >* newLine,
        κ("""  case x => s"$x is not valid """) *< string >* κ("""".asLeft""") *< newLine *< κ("}") *< newLine
      ).contramapN(x => flip(second(x)(_.map(x => x -> ident(x)))))
    ).contramap { case (x, xs) =>
      (x, Tpe[T](x), x -> xs)
    }

  def sumCirceEncoder[T: Basis[JsonSchemaF, *], B]: Printer[(String, List[String])] = {
    val polyObject: Printer[((String, Option[String]), List[PackageName], List[(String, String, String)])] = objectDef(
      sepBy(
        (
          κ("implicit def ") *< string >* κ(" = "),
          κ("at[") *< string >* κ("]"),
          κ("(x => Encoder[") *< string >* κ("].apply(x))")
        ).contramapN(identity),
        "\n"
      )
    )
    encoderDef[T, ((String, Option[String]), List[PackageName], List[(String, String, String)])](
      κ("Encoder.instance { x =>") *< newLine *< κ("import shapeless.Poly1") *< newLine *< polyObject >* newLine >* κ(
        "x.fold(json)"
      ) >* newLine >* κ("}")
    ).contramap { case (x, xs) =>
      (x, Tpe[T](x), (("json", "Poly1".some), List.empty, xs.map(x => (x.toLowerCase(), x, x))))
    }
  }

  def sumCirceDecoder[T: Basis[JsonSchemaF, *], B]: Printer[(String, List[String])] =
    decoderDef[T, (List[(String, String)])](
      sepBy(
        divBy(κ("Decoder[") *< string >* κ("]"), κ(".map(x => "), κ("Coproduct[") *< string >* κ("](x))")),
        " orElse "
      )
    ).contramap { case (x, xs) => (x, Tpe[T](x), xs.map(_ -> x)) }

  def circeDecoder[T: Basis[JsonSchemaF, *]]: Printer[(String, Tpe[T])] =
    decoderDef(κ("deriveDecoder[") *< tpe[T] >* κ("]"))
      .contramap(x => (x._1, x._2, x._2))

  def circeEncoder[T: Basis[JsonSchemaF, *]]: Printer[(String, Tpe[T])] =
    encoderDef(κ("deriveEncoder[") *< tpe[T] >* κ("]"))
      .contramap(x => (x._1, x._2, x._2))

  def forProductCirceEncoder[T: Basis[JsonSchemaF, *]]: Printer[(String, Tpe[T], List[String])] =
    encoderDef[T, List[String]](
      (
        κ("Encoder.forProduct") *< string >* κ("("),
        sepBy(κ("\"") *< string >* κ("\""), ", ") >* κ(")"),
        κ("(t => (") *< sepBy(κ("t.") *< show[Var], ", ") >* κ("))")
      ).contramapN(x => (x.length.toString, x, x.map(Var.apply)))
    ).contramap(x => (x._1, x._2, x._3))

  def forProductCirceDecoder[T: Basis[JsonSchemaF, *]]: Printer[(String, Tpe[T], List[String])] =
    decoderDef[T, (List[String], Tpe[T])](
      (
        κ("Decoder.forProduct") *< string >* κ("("),
        sepBy(κ("\"") *< string >* κ("\""), ", ") >* κ(")"),
        κ("(") *< tpe[T] >* κ(".apply)")
      ).contramapN(x => (x._1.length.toString, x._1, x._2))
    ).contramap(x => (x._1, x._2, (x._3, x._2)))

  def entityDecoder[T: Basis[JsonSchemaF, *]]: Printer[(String, Tpe[T])] =
    (
      κ("implicit def ") *< string >* κ("EntityDecoder[F[_]:Sync]: "),
      κ("EntityDecoder[F, ") *< tpe[T] >* κ("] = "),
      κ("jsonOf[F, ") *< tpe[T] >* κ("]")
    ).contramapN(x => (normalize(x._1), x._2, x._2))

  def entityEncoder[T: Basis[JsonSchemaF, *]]: Printer[(String, Tpe[T])] =
    (
      κ("implicit def ") *< string >* κ("EntityEncoder[F[_]:Applicative]: "),
      κ("EntityEncoder[F, ") *< tpe[T] >* κ("] = "),
      κ("jsonEncoderOf[F, ") *< tpe[T] >* κ("]")
    ).contramapN(x => (normalize(x._1), x._2, x._2))

  private def showEnum[T: Basis[JsonSchemaF, *]]: Printer[((String, String), List[(String, String)])] =
    divBy(
      implicitVal(κ("Show.show {")).contramap { case (x, y) => (normalize(x), "Show", Tpe[T](y), ()) },
      newLine,
      sepBy(
        divBy(space *< space *< κ("case ") *< string, κ(" => "), κ("\"") *< string >* κ("\"")),
        "\n"
      ) >* newLine >* κ(
        "}"
      )
    )

}
