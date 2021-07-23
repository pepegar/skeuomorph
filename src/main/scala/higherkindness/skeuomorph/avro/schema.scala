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

package higherkindness.skeuomorph.avro

import cats.kernel.Eq
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.instances.int._
import cats.syntax.eq._
import higherkindness.droste.macros.deriveTraverse
import io.circe.Json
import org.apache.avro.{LogicalType, LogicalTypes, Schema}
import org.apache.avro.Schema.Type
import higherkindness.droste.{Algebra, Coalgebra}

import scala.jdk.CollectionConverters._

@deriveTraverse sealed trait AvroF[A]
object AvroF {

  def order2Order(avroO: Schema.Field.Order): Order =
    avroO match {
      case Schema.Field.Order.ASCENDING  => Order.Ascending
      case Schema.Field.Order.DESCENDING => Order.Descending
      case Schema.Field.Order.IGNORE     => Order.Ignore
    }

  def field2Field(avroF: Schema.Field): Field[Schema] =
    Field(
      avroF.name,
      avroF.aliases.asScala.toList,
      Option(avroF.doc),
      Option(order2Order(avroF.order)),
      avroF.schema
    )

  def field2Obj(f: Field[Json]): Json =
    Json.obj(
      "name"    -> Json.fromString(f.name),
      "aliases" -> Json.arr(f.aliases.map(Json.fromString): _*),
      "type"    -> f.tpe
    )

  sealed trait Order
  object Order {
    case object Ascending  extends Order
    case object Descending extends Order
    case object Ignore     extends Order

    implicit def orderEq: Eq[Order] =
      Eq.instance {
        case (Ascending, Ascending)   => true
        case (Descending, Descending) => true
        case (Ignore, Ignore)         => true
        case _                        => false
      }
  }

  @deriveTraverse final case class Field[A](
      name: String,
      aliases: List[String],
      doc: Option[String],
      order: Option[Order],
      tpe: A
  )

  object Field {
    implicit def fieldEq[T: Eq]: Eq[Field[T]] =
      Eq.instance {
        case (Field(n, a, d, o, t), Field(n2, a2, d2, o2, t2)) =>
          n === n2 && a === a2 && d === d2 && o === o2 && t === t2
        case _ => false
      }
  }

  final case class TNull[A]()                                     extends AvroF[A]
  final case class TBoolean[A]()                                  extends AvroF[A]
  final case class TInt[A]()                                      extends AvroF[A]
  final case class TLong[A]()                                     extends AvroF[A]
  final case class TFloat[A]()                                    extends AvroF[A]
  final case class TDouble[A]()                                   extends AvroF[A]
  final case class TBytes[A]()                                    extends AvroF[A]
  final case class TString[A]()                                   extends AvroF[A]
  final case class TNamedType[A](namespace: String, name: String) extends AvroF[A]
  final case class TArray[A](item: A)                             extends AvroF[A]
  final case class TMap[A](values: A)                             extends AvroF[A]
  final case class TRecord[A](
      name: String,
      namespace: Option[String],
      aliases: List[String],
      doc: Option[String],
      fields: List[Field[A]]
  ) extends AvroF[A]
  final case class TEnum[A](
      name: String,
      namespace: Option[String],
      aliases: List[String],
      doc: Option[String],
      symbols: List[String]
  )                                                                                                     extends AvroF[A]
  final case class TUnion[A](options: NonEmptyList[A])                                                  extends AvroF[A]
  final case class TFixed[A](name: String, namespace: Option[String], aliases: List[String], size: Int) extends AvroF[A]
  final case class TDate[A]()                                                                           extends AvroF[A]
  final case class TTimestampMillis[A]()                                                                extends AvroF[A]
  final case class TTimeMillis[A]()                                                                     extends AvroF[A]
  final case class TDecimal[A](precision: Int, scale: Int)                                              extends AvroF[A]

  implicit def eqAvroF[T: Eq]: Eq[AvroF[T]] =
    Eq.instance {
      case (TNull(), TNull())                             => true
      case (TBoolean(), TBoolean())                       => true
      case (TInt(), TInt())                               => true
      case (TLong(), TLong())                             => true
      case (TFloat(), TFloat())                           => true
      case (TDouble(), TDouble())                         => true
      case (TBytes(), TBytes())                           => true
      case (TString(), TString())                         => true
      case (TNamedType(ns, n), TNamedType(ns2, n2))       => ns === ns2 && n === n2
      case (TArray(i), TArray(i2))                        => i === i2
      case (TMap(v), TMap(v2))                            => v === v2
      case (TUnion(o), TUnion(o2))                        => o === o2
      case (TFixed(n, ns, a, s), TFixed(n2, ns2, a2, s2)) => n === n2 && ns === ns2 && a === a2 && s === s2
      case (TRecord(n, ns, al, d, f), TRecord(n2, ns2, al2, d2, f2)) =>
        n === n2 && ns === ns2 && al === al2 && d === d2 && f === f2
      case (TEnum(n, ns, al, d, s), TEnum(n2, ns2, al2, d2, s2)) =>
        n === n2 && ns === ns2 && al === al2 && d === d2 && s === s2
      case _ => false
    }

  /**
   * Helper methods to construct AvroF values.  These methods are to
   * avoid scala infering the case type instead of AvroF.
   */
  def `null`[A](): AvroF[A]                                   = TNull[A]()
  def boolean[A](): AvroF[A]                                  = TBoolean[A]()
  def int[A](): AvroF[A]                                      = TInt[A]()
  def long[A](): AvroF[A]                                     = TLong[A]()
  def float[A](): AvroF[A]                                    = TFloat[A]()
  def double[A](): AvroF[A]                                   = TDouble[A]()
  def bytes[A](): AvroF[A]                                    = TBytes[A]()
  def string[A](): AvroF[A]                                   = TString[A]()
  def namedType[A](namespace: String, name: String): AvroF[A] = TNamedType[A](namespace, name)
  def array[A](item: A): AvroF[A]                             = TArray[A](item)
  def map[A](values: A): AvroF[A]                             = TMap[A](values)
  def record[A](
      name: String,
      namespace: Option[String],
      aliases: List[String],
      doc: Option[String],
      fields: List[Field[A]]
  ): AvroF[A] = TRecord(name, namespace, aliases, doc, fields)
  def enum[A](
      name: String,
      namespace: Option[String],
      aliases: List[String],
      doc: Option[String],
      symbols: List[String]
  ): AvroF[A]                                      = TEnum(name, namespace, aliases, doc, symbols)
  def union[A](options: NonEmptyList[A]): AvroF[A] = TUnion(options)
  def fixed[A](name: String, namespace: Option[String], aliases: List[String], size: Int): AvroF[A] =
    TFixed(name, namespace, aliases, size)

  /**
   * Convert org.apache.avro.Schema to skeuomorph.avro.Schema
   */
  def fromAvro: Coalgebra[AvroF, Schema] =
    Coalgebra { sch =>
      Option(sch.getLogicalType) match {
        case Some(lt) => logicalType(lt)
        case None     => primitiveType(sch)
      }
    }

  private def logicalType(logicalType: LogicalType): AvroF[Schema] = logicalType match {
    case _: LogicalTypes.Date            => AvroF.TDate()
    case _: LogicalTypes.TimestampMillis => AvroF.TTimestampMillis()
    case _: LogicalTypes.TimeMillis      => AvroF.TTimeMillis()
    case dec: LogicalTypes.Decimal       => AvroF.TDecimal(dec.getPrecision, dec.getScale)
  }

  private def primitiveType(sch: Schema): AvroF[Schema] = sch.getType match {
    case Type.STRING  => AvroF.TString()
    case Type.BOOLEAN => AvroF.TBoolean()
    case Type.BYTES   => AvroF.TBytes()
    case Type.DOUBLE  => AvroF.TDouble()
    case Type.FLOAT   => AvroF.TFloat()
    case Type.INT     => AvroF.TInt()
    case Type.LONG    => AvroF.TLong()
    case Type.NULL    => AvroF.TNull()
    case Type.MAP     => AvroF.TMap(sch.getValueType)
    case Type.ARRAY   => AvroF.TArray(sch.getElementType)
    case Type.RECORD =>
      AvroF.TRecord(
        sch.getName,
        Option(sch.getNamespace),
        sch.getAliases.asScala.toList,
        Option(sch.getDoc),
        sch.getFields.asScala.toList.map(field2Field)
      )
    case Type.ENUM =>
      val symbols = sch.getEnumSymbols.asScala.toList
      AvroF.TEnum(
        sch.getName,
        Option(sch.getNamespace),
        sch.getAliases.asScala.toList,
        Option(sch.getDoc),
        symbols
      )
    case Type.UNION =>
      val types = sch.getTypes.asScala.toList
      AvroF.TUnion(
        NonEmptyList.fromListUnsafe(types)
      )
    case Type.FIXED =>
      AvroF.TFixed(
        sch.getName,
        Option(sch.getNamespace),
        sch.getAliases.asScala.toList,
        sch.getFixedSize
      )
  }

  def toJson: Algebra[AvroF, Json] =
    Algebra {
      case TNull()                     => Json.fromString("Null")
      case TBoolean()                  => Json.fromString("Boolean")
      case TInt()                      => Json.fromString("Int")
      case TLong()                     => Json.fromString("Long")
      case TFloat()                    => Json.fromString("Float")
      case TDouble()                   => Json.fromString("Double")
      case TBytes()                    => Json.fromString("Bytes")
      case TString()                   => Json.fromString("String")
      case TNamedType(namespace, name) => Json.fromString(s"$namespace.$name")
      case TArray(item) =>
        Json.obj(
          "type"  -> Json.fromString("array"),
          "items" -> item
        )
      case TMap(values) =>
        Json.obj(
          "type"   -> Json.fromString("map"),
          "values" -> values
        )
      case TRecord(name, namespace, aliases, doc, fields) =>
        val base: Json = Json.obj(
          "type"   -> Json.fromString("record"),
          "name"   -> Json.fromString(name),
          "fields" -> Json.arr(fields.map(field2Obj): _*)
        )
        val withNamespace = namespace.fold(base)(n => base deepMerge Json.obj("namespace" -> Json.fromString(n)))
        val withAliases =
          if (aliases.isEmpty)
            withNamespace
          else
            withNamespace deepMerge Json.obj("aliases" -> Json.arr(aliases.map(Json.fromString): _*))
        val withDoc = doc.fold(withAliases)(f => withAliases deepMerge Json.obj("doc" -> Json.fromString(f)))
        withDoc
      case TEnum(_, _, _, _, _) => ???
      case TUnion(options)      => Json.arr(options.toList: _*)
      case TFixed(name, _, _, size) =>
        Json.obj(
          "type" -> Json.fromString("fixed"),
          "name" -> Json.fromString(name),
          "size" -> Json.fromInt(size)
        )
      case TDate() =>
        Json.obj(
          "type"        -> Json.fromString("int"),
          "logicalType" -> Json.fromString("date")
        )
      case TTimestampMillis() =>
        Json.obj(
          "type"        -> Json.fromString("long"),
          "logicalType" -> Json.fromString("timestamp-millis")
        )
      case TTimeMillis() =>
        Json.obj(
          "type"        -> Json.fromString("int"),
          "logicalType" -> Json.fromString("time-millis")
        )
      case TDecimal(precision, scale) =>
        Json.obj(
          "type"        -> Json.fromString("bytes"),
          "logicalType" -> Json.fromString("decimal"),
          "precision"   -> Json.fromInt(precision),
          "scale"       -> Json.fromInt(scale)
        )
    }
}
