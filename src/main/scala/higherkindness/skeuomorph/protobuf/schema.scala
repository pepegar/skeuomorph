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

package higherkindness.skeuomorph.protobuf

import cats.{Applicative, Eq}
import cats.data.NonEmptyList
import cats.syntax.all._
import higherkindness.skeuomorph.protobuf.ProtobufF.OptionValue
import higherkindness.droste.util.DefaultTraverse

sealed trait FieldF[A] {
  val name: String
  val tpe: A
  // this is a list because a oneof field will have one index per branch
  def indices: List[Int]
}

object FieldF {
  final case class Field[A](
      name: String,
      tpe: A,
      position: Int,
      options: List[OptionValue],
      isRepeated: Boolean,
      isMapField: Boolean
  ) extends FieldF[A] {
    def indices: List[Int] = List(position)
  }

  final case class OneOfField[A](name: String, tpe: A, indices: List[Int]) extends FieldF[A]

  implicit def fieldEq[T: Eq]: Eq[FieldF[T]] =
    Eq.instance {
      case (Field(n, t, p, o, r, m), Field(n2, t2, p2, o2, r2, m2)) =>
        n === n2 && t === t2 && p === p2 && o === o2 && r === r2 && m === m2
      case (OneOfField(n, tpe, is), OneOfField(n2, tpe2, is2)) =>
        n === n2 && tpe === tpe2 && is === is2
      case _ => false
    }
}

sealed trait IntModifier extends Product with Serializable
case object Signed       extends IntModifier
case object Unsigned     extends IntModifier
case object FixedWidth   extends IntModifier

sealed trait ProtobufF[A]

object ProtobufF {

  final case class OptionValue(name: String, value: String)
  object OptionValue {
    implicit val optionEq: Eq[OptionValue] = Eq.instance { case (OptionValue(n, v), OptionValue(n2, v2)) =>
      n === n2 && v === v2
    }
  }

  final case class TNull[A]()                                                     extends ProtobufF[A]
  final case class TDouble[A]()                                                   extends ProtobufF[A]
  final case class TFloat[A]()                                                    extends ProtobufF[A]
  final case class TInt32[A]()                                                    extends ProtobufF[A]
  final case class TInt64[A]()                                                    extends ProtobufF[A]
  final case class TUint32[A]()                                                   extends ProtobufF[A]
  final case class TUint64[A]()                                                   extends ProtobufF[A]
  final case class TSint32[A]()                                                   extends ProtobufF[A]
  final case class TSint64[A]()                                                   extends ProtobufF[A]
  final case class TFixed32[A]()                                                  extends ProtobufF[A]
  final case class TFixed64[A]()                                                  extends ProtobufF[A]
  final case class TSfixed32[A]()                                                 extends ProtobufF[A]
  final case class TSfixed64[A]()                                                 extends ProtobufF[A]
  final case class TBool[A]()                                                     extends ProtobufF[A]
  final case class TString[A]()                                                   extends ProtobufF[A]
  final case class TBytes[A]()                                                    extends ProtobufF[A]
  final case class TNamedType[A](prefix: List[String], name: String)              extends ProtobufF[A]
  final case class TOptionalNamedType[A](prefix: List[String], name: String)      extends ProtobufF[A]
  final case class TRepeated[A](value: A)                                         extends ProtobufF[A]
  final case class TOneOf[A](name: String, fields: NonEmptyList[FieldF.Field[A]]) extends ProtobufF[A]
  final case class TMap[A](keyTpe: A, value: A)                                   extends ProtobufF[A]
  final case class TEnum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[OptionValue],
      aliases: List[(String, Int)]
  ) extends ProtobufF[A]
  final case class TMessage[A](
      name: String,
      fields: List[FieldF[A]],
      reserved: List[List[String]],
      nestedMessages: List[A],
      nestedEnums: List[A]
  )                                                                                     extends ProtobufF[A]
  final case class TFileDescriptor[A](values: List[A], name: String, `package`: String) extends ProtobufF[A]

  def `null`[A](): ProtobufF[A]                                                   = TNull()
  def double[A](): ProtobufF[A]                                                   = TDouble()
  def float[A](): ProtobufF[A]                                                    = TFloat()
  def int32[A](): ProtobufF[A]                                                    = TInt32()
  def int64[A](): ProtobufF[A]                                                    = TInt64()
  def uint32[A](): ProtobufF[A]                                                   = TUint32()
  def uint64[A](): ProtobufF[A]                                                   = TUint64()
  def sint32[A](): ProtobufF[A]                                                   = TSint32()
  def sint64[A](): ProtobufF[A]                                                   = TSint64()
  def fixed32[A](): ProtobufF[A]                                                  = TFixed32()
  def fixed64[A](): ProtobufF[A]                                                  = TFixed64()
  def sfixed32[A](): ProtobufF[A]                                                 = TSfixed32()
  def sfixed64[A](): ProtobufF[A]                                                 = TSfixed64()
  def bool[A](): ProtobufF[A]                                                     = TBool()
  def string[A](): ProtobufF[A]                                                   = TString()
  def bytes[A](): ProtobufF[A]                                                    = TBytes()
  def namedType[A](prefix: List[String], name: String): ProtobufF[A]              = TNamedType(prefix, name)
  def optionalNamedType[A](prefix: List[String], name: String): ProtobufF[A]      = TOptionalNamedType(prefix, name)
  def repeated[A](value: A): ProtobufF[A]                                         = TRepeated(value)
  def oneOf[A](name: String, fields: NonEmptyList[FieldF.Field[A]]): ProtobufF[A] = TOneOf(name, fields)
  def map[A](keyTpe: A, value: A): ProtobufF[A]                                   = TMap(keyTpe, value)
  def enum[A](
      name: String,
      symbols: List[(String, Int)],
      options: List[OptionValue],
      aliases: List[(String, Int)]
  ): ProtobufF[A] = TEnum(name, symbols, options, aliases)
  def message[A](
      name: String,
      fields: List[FieldF[A]],
      reserved: List[List[String]],
      nestedMessages: List[A],
      nestedEnums: List[A]
  ): ProtobufF[A] =
    TMessage(name, fields, reserved, nestedMessages, nestedEnums)

  implicit def protobufEq[T: Eq]: Eq[ProtobufF[T]] =
    Eq.instance {
      case (TNull(), TNull())                                     => true
      case (TDouble(), TDouble())                                 => true
      case (TFloat(), TFloat())                                   => true
      case (TInt32(), TInt32())                                   => true
      case (TInt64(), TInt64())                                   => true
      case (TUint32(), TUint32())                                 => true
      case (TUint64(), TUint64())                                 => true
      case (TSint32(), TSint32())                                 => true
      case (TSint64(), TSint64())                                 => true
      case (TFixed32(), TFixed32())                               => true
      case (TFixed64(), TFixed64())                               => true
      case (TSfixed32(), TSfixed32())                             => true
      case (TSfixed64(), TSfixed64())                             => true
      case (TBool(), TBool())                                     => true
      case (TString(), TString())                                 => true
      case (TBytes(), TBytes())                                   => true
      case (TNamedType(p, n), TNamedType(p2, n2))                 => p === p2 && n === n2
      case (TOptionalNamedType(p, n), TOptionalNamedType(p2, n2)) => p === p2 && n === n2
      case (TRepeated(v), TRepeated(v2))                          => v === v2
      case (TEnum(n, s, o, a), TEnum(n2, s2, o2, a2))             => n === n2 && s === s2 && o === o2 && a === a2
      case (TMessage(n, f, r, nm, ne), TMessage(n2, f2, r2, nm2, ne2)) =>
        n === n2 && f === f2 && r === r2 && nm == nm2 && ne == ne2
      case _ => false
    }

  implicit val traverse: DefaultTraverse[ProtobufF] = new DefaultTraverse[ProtobufF] {
    def traverse[G[_], A, B](fa: ProtobufF[A])(f: A => G[B])(implicit G: Applicative[G]): G[ProtobufF[B]] = {

      def makeFieldB(field: FieldF.Field[A]): G[FieldF.Field[B]] =
        f(field.tpe).map(b =>
          FieldF.Field[B](field.name, b, field.position, field.options, field.isRepeated, field.isMapField)
        )

      def makeOneOfB(oneOf: FieldF.OneOfField[A]): G[FieldF[B]] =
        f(oneOf.tpe).map(b => FieldF.OneOfField[B](oneOf.name, b, oneOf.indices): FieldF[B])

      def traverseFieldF(fieldFList: List[FieldF[A]]): G[List[FieldF[B]]] =
        fieldFList.traverse {
          case field: FieldF.Field[A]      => makeFieldB(field).widen
          case oneOf: FieldF.OneOfField[A] => makeOneOfB(oneOf).widen
        }

      def makeMessageB(m: TMessage[A]): G[TMessage[B]] =
        (
          traverseFieldF(m.fields),
          m.nestedMessages.traverse(f),
          m.nestedEnums.traverse(f)
        ).mapN { case (bFields, bMsgs, bEnums) =>
          TMessage[B](m.name, bFields, m.reserved, bMsgs, bEnums)
        }

      fa match {
        case TNull()                          => `null`[B]().pure[G]
        case TDouble()                        => double[B]().pure[G]
        case TFloat()                         => float[B]().pure[G]
        case TInt32()                         => int32[B]().pure[G]
        case TInt64()                         => int64[B]().pure[G]
        case TUint32()                        => uint32[B]().pure[G]
        case TUint64()                        => uint64[B]().pure[G]
        case TSint32()                        => sint32[B]().pure[G]
        case TSint64()                        => sint64[B]().pure[G]
        case TFixed32()                       => fixed32[B]().pure[G]
        case TFixed64()                       => fixed64[B]().pure[G]
        case TSfixed32()                      => sfixed32[B]().pure[G]
        case TSfixed64()                      => sfixed64[B]().pure[G]
        case TBool()                          => bool[B]().pure[G]
        case TString()                        => string[B]().pure[G]
        case TBytes()                         => bytes[B]().pure[G]
        case TNamedType(prefix, name)         => namedType[B](prefix, name).pure[G]
        case TOptionalNamedType(prefix, name) => optionalNamedType[B](prefix, name).pure[G]
        case TRepeated(value)                 => f(value).map(TRepeated[B])
        case TOneOf(name, fields)             => fields.traverse(makeFieldB).map(bFields => TOneOf(name, bFields))
        case TMap(keyTpe, value)              => (f(keyTpe), f(value)).mapN(TMap[B])
        case TEnum(name, symbols, options, aliases) =>
          enum[B](name, symbols, options, aliases).pure[G]: G[ProtobufF[B]]
        case m: TMessage[A] => makeMessageB(m).widen
        case TFileDescriptor(values, name, p) =>
          values.traverse(f).map(bValues => TFileDescriptor(bValues, name, p))
      }
    }
  }
}
