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
import higherkindness.skeuomorph.Printer.{konst => κ, _}
import higherkindness.skeuomorph.catz.contrib.ContravariantMonoidalSyntax._
import higherkindness.skeuomorph.catz.contrib.Decidable._
import higherkindness.skeuomorph.openapi.JsonSchemaF.{string => _, _}
import cats.syntax.all._
import cats.Show

import higherkindness.droste._
import scala.util.matching.Regex

object print {
  import higherkindness.skeuomorph.openapi.schema.OpenApi

  val schemasRegex    = """#/components/schemas/(.+)""".r
  val parametersRegex = """#/components/parameters/(.+)""".r

  def schemaWithName[T: Basis[JsonSchemaF, *]](implicit codecs: Printer[Codecs]): Printer[(String, T)] =
    Printer {
      case (name, t) if isBasicType(t) => typeAliasDef(schema[T]()).print((ident(name), t, none))
      case (name, t) if isArray(t) =>
        typeAliasDef(schema[T]()).print((ident(name), t, none))
      case (name, t) => schema[T](ident(name).some).print(t)
    }

  protected[openapi] def schema[T: Basis[JsonSchemaF, *]](
      name: Option[String] = None
  )(implicit codecs: Printer[Codecs]): Printer[T] = {
    val listDef: Printer[String] = κ("List[") *< string >* κ("]")
    val algebra: Algebra[JsonSchemaF, String] = Algebra { x =>
      import JsonSchemaF._
      (x, name) match {
        case (IntegerF(), _)                                            => "Int"
        case (LongF(), _)                                               => "Long"
        case (FloatF(), _)                                              => "Float"
        case (DoubleF(), _)                                             => "Double"
        case (StringF(), _)                                             => "String"
        case (ByteF(), _)                                               => "Array[Byte]"
        case (BinaryF(), _)                                             => "List[Boolean]"
        case (BooleanF(), _)                                            => "Boolean"
        case (DateF(), _)                                               => "java.time.LocalDate"
        case (DateTimeF(), _)                                           => "java.time.ZonedDateTime"
        case (PasswordF(), _)                                           => "String"
        case (ObjectF(properties, _), Some(name)) if properties.isEmpty => s"type $name = io.circe.Json"
        case (ObjectF(properties, required), Some(name)) =>
          caseClassWithCodecsDef.print(
            (
              name,
              properties
                .map(x => x.name -> Tpe[T](x.tpe))
                .map { case (name, tpe) =>
                  if (required.contains(name))
                    name -> tpe
                  else
                    name -> tpe.copy(required = false)
                }
            )
          )

        case (ObjectF(properties, _), _) if properties.isEmpty => "io.circe.Json"
        case (ArrayF(x), _)                                    => listDef.print(x)
        case (EnumF(fields), Some(name))                       => sealedTraitDef.print(name -> fields)
        case (SumF(cases), Some(name))                         => sumDef.print(name -> cases)
        case (ReferenceF(schemasRegex(ref)), _)                => ident(ref)
        case (ReferenceF(parametersRegex(ref)), _)             => ident(ref)
        case (ReferenceF(ref), _)                              => ident(ref)
      }
    }
    Printer.print(scheme.cata(algebra))
  }

  def isBasicType[T: Basis[JsonSchemaF, *]](t: T): Boolean = {
    import JsonSchemaF._
    import higherkindness.droste.syntax.project.toProjectSyntaxOps
    t.project match {
      case ObjectF(p, _) => p.isEmpty
      case EnumF(_)      => false
      case ArrayF(_)     => false
      case ReferenceF(_) => false
      case SumF(_)       => false
      case _             => true
    }
  }

  def isReference[T: Basis[JsonSchemaF, *]](t: T)(regex: Regex): Boolean = {
    import JsonSchemaF._
    import higherkindness.droste.syntax.project.toProjectSyntaxOps
    t.project match {
      case ReferenceF(regex(_)) => true
      case _                    => false
    }

  }

  def isArray[T: Basis[JsonSchemaF, *]](t: T): Boolean = {
    import JsonSchemaF._
    import higherkindness.droste.syntax.project.toProjectSyntaxOps
    t.project match {
      case ArrayF(_) => true
      case _         => false
    }
  }
  def sumTypes[T: Basis[JsonSchemaF, *]](openApi: OpenApi[T]): List[String] = {
    def isSum(t: T): Boolean = {
      import JsonSchemaF._
      import higherkindness.droste.syntax.project.toProjectSyntaxOps
      t.project match {
        case SumF(_) => true
        case _       => false
      }
    }
    openApi.components.toList
      .flatMap(_.schemas.toList)
      .filter { case (_, t) => isSum(t) }
      .map { case (x, _) => ident(x) }
  }

  sealed trait Codecs
  final case class CaseClassCodecs(name: String, fields: List[String]) extends Codecs
  final case class EnumCodecs(name: String, values: List[String])      extends Codecs
  final case class SumCodecs(name: String, values: List[String])       extends Codecs

  final case class Tpe[T](tpe: Either[String, T], required: Boolean, description: String, nestedTypes: List[String])
  object Tpe {
    import Printer.avoid._
    def unit[T]: Tpe[T]                = Tpe("Unit".asLeft, true, "Unit", Nil)
    def apply[T](name: String): Tpe[T] = Tpe(name.asLeft, true, name, Nil)
    def apply[T](tpe: T, required: Boolean, description: String, nestedTypes: List[String]): Tpe[T] =
      Tpe(tpe.asRight, required, description, nestedTypes)

    def name[T: Basis[JsonSchemaF, *]](tpe: Tpe[T]): String =
      tpe.tpe.fold(
        identity,
        x =>
          tpe.nestedTypes.headOption.map(_ => s"${tpe.nestedTypes.mkString(".")}.").getOrElse("") +
            Printer
              .print(Optimize.namedTypes[T](ident(tpe.description)) >>> schema(none).print)
              .print(x)
      )
    def option[T: Basis[JsonSchemaF, *]](tpe: Tpe[T]): Either[String, String] =
      if (tpe.required)
        name(tpe).asRight
      else
        name(tpe).asLeft
  }

  final case class PackageName(value: String) extends AnyVal
  object PackageName {
    implicit val packageNameShow: Show[PackageName] = Show.show(_.value)
  }

  final case class Var(name: String) extends AnyVal
  object Var {
    implicit val varShow: Show[Var] = Show.show(x => decapitalize(ident(x.name)))
  }
  final case class VarWithType[T](name: Var, tpe: Tpe[T])
  object VarWithType {
    def tpe[T: Basis[JsonSchemaF, *]](tpe: Tpe[T]): VarWithType[T] = VarWithType(Var(Tpe.name(tpe)), tpe)
    def apply[T](
        name: String,
        tpe: T,
        required: Boolean,
        description: String,
        nestedTypes: List[String]
    ): VarWithType[T] =
      VarWithType(Var(name), Tpe(tpe.asRight, required, description, nestedTypes))
  }

  def model[T: Basis[JsonSchemaF, *]](implicit codecs: Printer[Codecs]): Printer[OpenApi[T]] =
    optional(objectDef(sepBy(schemaWithName, "\n"))).contramap { x =>
      val models = x.components.toList.flatMap(_.schemas)
      models.headOption.map(_ =>
        (
          ("models", none),
          (List("shapeless.{:+:, CNil}", "shapeless.Coproduct") ++ sumTypes(x).map(x => s"$x._"))
            .map(PackageName.apply),
          x.components.toList.flatMap(_.schemas)
        )
      )
    }

  private def sumDef(implicit codecs: Printer[Codecs]): Printer[(String, List[String])] =
    divBy(
      divBy(κ("type ") *< string, κ(" = "), sepBy(string, " :+: ") >* κ(" :+: CNil")),
      newLine,
      objectDef(codecs)
    ).contramap(x => (x, ((x._1, none), List.empty, SumCodecs.apply _ tupled x)))

  private def caseObjectDef: Printer[(String, String)] =
    (κ("final case object ") *< string >* κ(" extends "), string).contramapN { case (x, y) => (ident(x), y) }

  private def sealedTraitCompanionObjectDef(implicit
      codecs: Printer[Codecs]
  ): Printer[(List[(String, String)], Codecs)] =
    divBy(sepBy(space *< space *< caseObjectDef, "\n"), newLine, codecs)

  private def sealedTraitDef(implicit codecs: Printer[Codecs]): Printer[(String, List[String])] =
    divBy(κ("sealed trait ") *< string, newLine, objectDef(sealedTraitCompanionObjectDef))
      .contramap { case (name, fields) =>
        (name, ((name, none), List.empty, (fields.map(_ -> name), EnumCodecs(name, fields))))
      }

  def caseClassDef[T: Basis[JsonSchemaF, *]]: Printer[(String, List[(String, Tpe[T])])] =
    (κ("final case class ") *< string, κ("(") *< sepBy(argumentDef[T], ", ") >* κ(")")).contramapN { case (x, y) =>
      x -> y.map { case (x, y) => VarWithType(Var(x), y) }
    }

  def caseClassWithCodecsDef[T: Basis[JsonSchemaF, *], A](implicit
      codecs: Printer[Codecs]
  ): Printer[(String, List[(String, Tpe[T])])] =
    (caseClassDef[T], optional(newLine *< objectDef(codecs))).contramapN { x =>
      ((x._1 -> x._2), ((x._1, none), List.empty[PackageName], CaseClassCodecs(x._1, x._2.map(_._1))).some)
    }

  def typeAliasDef[T](
      typeSchemaDef: Printer[T]
  )(implicit codecs: Printer[Codecs]): Printer[(String, T, Option[(List[PackageName], Codecs)])] =
    (κ("type ") *< string >* κ(" = "), typeSchemaDef, optional(newLine *< objectDef(codecs))).contramapN {
      case (name, tpe, codecInfo) =>
        (name, tpe, codecInfo.map { case (x, y) => ((name, none), x, y) })
    }

  def implicitVal[T: Basis[JsonSchemaF, *], A](body: Printer[A]): Printer[(String, String, Tpe[T], A)] =
    (κ("implicit val ") *< string, string >* κ(": "), divBy(string, κ("["), tpe[T] >* κ("] = ")), body)
      .contramapN { case (a, b, c, d) => (a, b, (b, c), d) }

  def objectDef[A](body: Printer[A]): Printer[((String, Option[String]), List[PackageName], A)] =
    divBy(
      divBy(
        divBy(κ("object ") *< string, κ(" "), optional(κ("extends ") *< string >* κ(" ")) >* κ("{")),
        newLine,
        sepBy(importDef, "\n")
      ),
      newLine,
      body >* newLine *< κ("}")
    ).contramap { case (x, y, z) => (x -> y, z) }

  def normalize(value: String): String = {
    val withoutBackticks = value.stripPrefix("`").stripSuffix("`")
    withoutBackticks
      .dropWhile(_.isDigit)
      .split("[ _-]")
      .map(_.filter(x => x.isLetterOrDigit).capitalize)
      .mkString ++ withoutBackticks.takeWhile(_.isDigit)
  }

  def ident(value: String): String = Printer.toValidIdentifier(value)

  def divBy[A, B](p1: Printer[A], sep: Printer[Unit], p2: Printer[B]): Printer[(A, B)] =
    (p1, sep, p2).contramapN[(A, B)] { case (x, y) => (x, (), y) }

  def tpe[T: Basis[JsonSchemaF, *]]: Printer[Tpe[T]] =
    ((κ("Option[") *< string >* κ("]")) >|< string)
      .contramap(Tpe.option[T])

  def importDef: Printer[PackageName] =
    (κ("import ") *< show[PackageName]).contramap(identity)

  def argumentDef[T: Basis[JsonSchemaF, *]]: Printer[VarWithType[T]] =
    divBy(show[Var], κ(": "), tpe).contramap(x => x.name -> x.tpe)

  def un[A, B, C, D](pair: ((A, B), (C, D))): (A, B, C, D) = (pair._1._1, pair._1._2, pair._2._1, pair._2._2)
  def un[A, C, D](pair: (A, (C, D))): (A, C, D)            = (pair._1, pair._2._1, pair._2._2)
  def duplicate[A, B](pair: (A, B)): ((A, A), (B, B))      = (pair._1 -> pair._1, pair._2 -> pair._2)
  def second[A, B, C](pair: (A, B))(f: B => C): (A, C)     = (pair._1, f(pair._2))
  def flip[A, B](pair: (A, B)): (B, A)                     = (pair._2, pair._1)
  def decapitalize(s: String): String                      = if (s.isEmpty) s else s"${s(0).toLower}${s.substring(1)}"

}
