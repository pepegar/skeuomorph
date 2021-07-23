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

package higherkindness.skeuomorph

import java.util.UUID

import cats.data.NonEmptyList
import cats.syntax.all._
import org.apache.avro.Schema
import org.scalacheck._
import org.scalacheck.cats.implicits._
import mu.MuF
import avro.AvroF
import protobuf._
import openapi._
import openapi.schema.OpenApi
import higherkindness.droste._
import scala.jdk.CollectionConverters._

object instances {
  lazy val nonEmptyString: Gen[String] = Gen.alphaStr.filter(_.nonEmpty)

  lazy val smallNumber: Gen[Int] = Gen.choose(1, 10)

  lazy val sampleBool: Gen[Boolean] = Gen.oneOf(true, false)

  implicit val avroSchemaArbitrary: Arbitrary[Schema] = Arbitrary {
    val primitives: Gen[Schema] = Gen.oneOf(
      List(
        org.apache.avro.Schema.Type.STRING,
        org.apache.avro.Schema.Type.BOOLEAN,
        org.apache.avro.Schema.Type.BYTES,
        org.apache.avro.Schema.Type.DOUBLE,
        org.apache.avro.Schema.Type.FLOAT,
        org.apache.avro.Schema.Type.INT,
        org.apache.avro.Schema.Type.LONG,
        org.apache.avro.Schema.Type.NULL
      ).map(Schema.create)
    )

    val arrayOrMap: Gen[Schema] =
      Gen.oneOf(primitives.map(Schema.createMap), primitives.map(Schema.createArray))

    val union: Gen[Schema] =
      Gen.nonEmptyContainerOf[Set, Schema](primitives).map(l => Schema.createUnion(l.toList.asJava))

    def field(name: String): Gen[Schema.Field] =
      for {
        schema <- Gen.oneOf(primitives, arrayOrMap, union)
        doc    <- nonEmptyString
      } yield new Schema.Field(name, schema, doc, null.asInstanceOf[Any])

    val record: Gen[Schema] = (
      nonEmptyString,
      nonEmptyString,
      nonEmptyString,
      Gen.nonEmptyContainerOf[Set, String](nonEmptyString).map(_.toList) flatMap { l: List[String] =>
        l.traverse(field)
      }
    ).mapN { case (name, doc, namespace, fields) =>
      Schema.createRecord(name, doc, namespace, false, fields.asJava)
    }

    Gen.oneOf(primitives, arrayOrMap, union, record)
  }

  implicit def muCoproductArbitrary[T](withTNull: Boolean)(implicit B: Basis[MuF, T]): Arbitrary[MuF.TCoproduct[T]] =
    Arbitrary {
      val nonNullPrimitives: Gen[MuF[T]] = Gen.oneOf(
        List(
          MuF.TString[T](),
          MuF.TBoolean[T](),
          MuF.TByteArray[T](MuF.Length.Arbitrary),
          MuF.TDouble[T](),
          MuF.TFloat[T](),
          MuF.int[T](),
          MuF.long[T]()
        )
      )

      (
        nonNullPrimitives,
        if (withTNull) Gen.const(MuF.TNull[T]()) else nonNullPrimitives,
        sampleBool
      ).mapN((t1, t2, reversed) =>
        MuF.TCoproduct(
          if (reversed) NonEmptyList.of(B.algebra(t2), B.algebra(t1))
          else NonEmptyList.of(B.algebra(t1), B.algebra(t2))
        )
      )
    }

  implicit def muArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[MuF[T]] = {

    def fieldGen: Gen[MuF.Field[T]] =
      (
        nonEmptyString,
        Gen.lzy(T.arbitrary),
        Gen.posNum[Int].map(i => Some(List(i)))
      ).mapN(MuF.Field.apply)
    def lengthGen: Gen[MuF.Length] = {
      val fixedGen = for {
        posInt    <- Gen.posNum[Int]
        name      <- Gen.alphaStr
        namespace <- Gen.option(Gen.alphaStr)
      } yield MuF.Length.Fixed(name, namespace, posInt)

      Gen.oneOf(MuF.Length.Arbitrary.pure[Gen], fixedGen)
    }

    Arbitrary(
      Gen.oneOf(
        MuF.`null`[T]().pure[Gen],
        MuF.double[T]().pure[Gen],
        MuF.float[T]().pure[Gen],
        MuF.int[T]().pure[Gen],
        MuF.long[T]().pure[Gen],
        MuF.boolean[T]().pure[Gen],
        MuF.string[T]().pure[Gen],
        lengthGen.map(l => MuF.byteArray[T](l)),
        (Gen.listOf(nonEmptyString), nonEmptyString) mapN MuF.namedType[T],
        T.arbitrary map MuF.option[T],
        (T.arbitrary, T.arbitrary) mapN { (a, b) => MuF.either(a, b) },
        T.arbitrary map MuF.list[T],
        T.arbitrary map (t => MuF.map[T](None, t)),
        T.arbitrary map MuF.required[T],
        (T.arbitrary, Gen.listOf(T.arbitrary)) mapN { (a, b) => MuF.generic[T](a, b) },
        (T.arbitrary, Gen.listOf(T.arbitrary)) mapN { (a, b) => MuF.generic[T](a, b) },
        Gen.nonEmptyListOf(T.arbitrary) map { l => MuF.coproduct[T](NonEmptyList.fromListUnsafe(l)) },
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.nonEmptyListOf(Gen.lzy(fieldGen)),
          Gen.listOfN(1, T.arbitrary),
          Gen.listOfN(1, T.arbitrary)
        ).mapN((n, ns, f, p, c) => MuF.product(n, ns, f, p, c))
      )
    )
  }

  def eitherGen[A, B](left: Gen[A], right: Gen[B]): Gen[Either[A, B]] =
    Gen.oneOf(left.map(_.asLeft[B]), right.map(_.asRight[A]))

  implicit def openApiArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[OpenApi[T]] = {
    import openapi.schema._
    // TODO: after Scalacheck 1.14.1 upgrade this generator doesn't work anymore
    //def mapStringToGen[A](gen: Gen[A]): Gen[Map[String, A]] = Gen.mapOfN(2, Gen.zip(nonEmptyString.map(_.take(4)), gen))
    def mapStringToGen[A](gen: Gen[A]): Gen[Map[String, A]] = gen.map(a => Map(UUID.randomUUID.toString.take(4) -> a))
    val optionStringGen                                     = Gen.option(nonEmptyString)
    val infoGen: Gen[Info]                                  = (nonEmptyString, optionStringGen, nonEmptyString).mapN(Info)
    val serverVariableGen: Gen[Server.Variable] =
      (Gen.listOfN(2, nonEmptyString), nonEmptyString, optionStringGen).mapN(Server.Variable)
    val serverGen: Gen[Server] =
      (nonEmptyString, Gen.option(nonEmptyString), mapStringToGen(serverVariableGen))
        .mapN(Server.apply)
    val serversGen                         = Gen.listOfN(2, serverGen)
    val externalDocsGen: Gen[ExternalDocs] = (nonEmptyString, optionStringGen).mapN(ExternalDocs)
    val tagGen: Gen[Tag]                   = (nonEmptyString, optionStringGen, Gen.option(externalDocsGen)).mapN(Tag)
    val referenceGen: Gen[Reference]       = nonEmptyString map Reference
    val mediaTypeGen: Gen[MediaType[T]] =
      (Gen.option(T.arbitrary), Map.empty[String, Encoding[T]].pure[Gen]).mapN(MediaType.apply)
    val headerGen: Gen[Header[T]]   = (nonEmptyString, T.arbitrary).mapN(Header.apply)
    val requestGen: Gen[Request[T]] = (optionStringGen, mapStringToGen(mediaTypeGen), sampleBool).mapN(Request.apply)
    val responseGen: Gen[Response[T]] =
      (nonEmptyString, mapStringToGen(eitherGen(headerGen, referenceGen)), mapStringToGen(mediaTypeGen))
        .mapN(Response.apply)
    val responsesGen = mapStringToGen(eitherGen(responseGen, referenceGen))

    val locationGen: Gen[Location] = Gen.oneOf(Location.all)
    val parameterGen: Gen[Parameter[T]] =
      (
        nonEmptyString,
        locationGen,
        Gen.option(nonEmptyString),
        Gen.option(sampleBool),
        Gen.option(sampleBool),
        Gen.option(nonEmptyString),
        Gen.option(sampleBool),
        Gen.option(sampleBool),
        Gen.option(sampleBool),
        T.arbitrary
      ).mapN(Parameter.apply)

    val parameters: Gen[List[Either[Parameter[T], Reference]]] = Gen.listOfN(2, eitherGen(parameterGen, referenceGen))

    val operationGen: Gen[Path.Operation[T]] =
      (
        Gen.listOfN(2, nonEmptyString),
        Gen.option(nonEmptyString),
        Gen.option(nonEmptyString),
        Gen.option(externalDocsGen),
        Gen.option(nonEmptyString),
        parameters,
        Gen.option(eitherGen(requestGen, referenceGen)),
        responsesGen,
        Map.empty[String, Either[Callback[T], Reference]].pure[Gen],
        sampleBool,
        serversGen
      ).mapN(Path.Operation.apply)

    val itemObjectsGen: Gen[Path.ItemObject[T]] =
      (
        Gen.option(nonEmptyString),
        Gen.option(nonEmptyString),
        Gen.option(nonEmptyString),
        Gen.option(operationGen),
        Gen.option(operationGen),
        Gen.option(operationGen),
        Gen.option(operationGen),
        Gen.option(operationGen),
        Gen.option(operationGen),
        Gen.option(operationGen),
        Gen.option(operationGen),
        serversGen,
        parameters
      ).mapN(Path.ItemObject.apply)

    val componentsGen: Gen[Components[T]] =
      (
        mapStringToGen(T.arbitrary),
        responsesGen,
        mapStringToGen(eitherGen(requestGen, referenceGen)),
        mapStringToGen(eitherGen(parameterGen, referenceGen))
      ).mapN(Components.apply)

    Arbitrary(
      (
        nonEmptyString,
        infoGen,
        Gen.listOfN(2, serverGen),
        mapStringToGen(itemObjectsGen),
        Gen.option(componentsGen),
        Gen.listOfN(5, tagGen),
        Gen.option(externalDocsGen)
      ).mapN(OpenApi[T])
    )
  }

  implicit def jsonSchemaFOpenApiArbitrary: Arbitrary[JsonSchemaF.Fixed] = {
    import JsonSchemaF.Fixed

    val basicGen: Gen[JsonSchemaF.Fixed] = Gen.oneOf(
      Fixed.integer().pure[Gen],
      Fixed.long().pure[Gen],
      Fixed.float().pure[Gen],
      Fixed.double().pure[Gen],
      Fixed.string().pure[Gen],
      Fixed.byte().pure[Gen],
      Fixed.binary().pure[Gen],
      Fixed.boolean().pure[Gen],
      Fixed.date().pure[Gen],
      Fixed.dateTime().pure[Gen],
      Fixed.password().pure[Gen],
      Gen.listOfN(2, nonEmptyString) map Fixed.enum,
      nonEmptyString map Fixed.reference
    )

    def rec(depth: Int): Gen[JsonSchemaF.Fixed] =
      depth match {
        case 1 => basicGen
        case n =>
          Gen.oneOf(
            rec(n - 1) map Fixed.array,
            Gen.listOfN(3, Gen.zip(nonEmptyString, rec(n - 1))) map { n =>
              Fixed.`object`(n, n.take(n.size - 1).map(_._1))
            }
          )
      }

    Arbitrary(rec(2))
  }

  implicit def jsonSchemaOpenApiArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[JsonSchemaF[T]] = {
    val propertyGen: Gen[JsonSchemaF.Property[T]] = (nonEmptyString, T.arbitrary).mapN(JsonSchemaF.Property[T])
    val objectGen: Gen[JsonSchemaF[T]] = (
      Gen.listOf(propertyGen),
      Gen.listOf(nonEmptyString)
    ).mapN(JsonSchemaF.`object`[T])

    Arbitrary(
      Gen.oneOf(
        JsonSchemaF.integer[T]().pure[Gen],
        JsonSchemaF.long[T]().pure[Gen],
        JsonSchemaF.float[T]().pure[Gen],
        JsonSchemaF.double[T]().pure[Gen],
        JsonSchemaF.string[T]().pure[Gen],
        JsonSchemaF.byte[T]().pure[Gen],
        JsonSchemaF.binary[T]().pure[Gen],
        JsonSchemaF.boolean[T]().pure[Gen],
        JsonSchemaF.date[T]().pure[Gen],
        JsonSchemaF.dateTime[T]().pure[Gen],
        JsonSchemaF.password[T]().pure[Gen],
        T.arbitrary map JsonSchemaF.array,
        Gen.listOf(nonEmptyString) map JsonSchemaF.enum[T],
        Gen.listOf(T.arbitrary) map JsonSchemaF.sum[T],
        objectGen,
        nonEmptyString.map(JsonSchemaF.reference[T])
      )
    )
  }

  implicit def avroArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[AvroF[T]] = {

    val orderGen: Gen[AvroF.Order] = Gen.oneOf(AvroF.Order.Ascending, AvroF.Order.Descending, AvroF.Order.Ignore)

    val fieldGen: Gen[AvroF.Field[T]] = (
      nonEmptyString,
      Gen.listOf(nonEmptyString),
      Gen.option(nonEmptyString),
      Gen.option(orderGen),
      T.arbitrary
    ).mapN(AvroF.Field.apply[T])

    Arbitrary(
      Gen.oneOf(
        AvroF.`null`[T]().pure[Gen],
        AvroF.boolean[T]().pure[Gen],
        AvroF.int[T]().pure[Gen],
        AvroF.long[T]().pure[Gen],
        AvroF.float[T]().pure[Gen],
        AvroF.double[T]().pure[Gen],
        AvroF.bytes[T]().pure[Gen],
        AvroF.string[T]().pure[Gen],
        (nonEmptyString, nonEmptyString).mapN(AvroF.namedType[T]),
        T.arbitrary map AvroF.array[T],
        T.arbitrary map AvroF.map[T],
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.option(nonEmptyString),
          Gen.listOf(fieldGen)
        ).mapN(AvroF.record[T]),
        Gen.nonEmptyListOf(T.arbitrary) map { l => AvroF.union[T](NonEmptyList.fromListUnsafe(l)) },
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString)
        ).mapN(AvroF.enum[T]),
        (
          nonEmptyString,
          Gen.option(nonEmptyString),
          Gen.listOf(nonEmptyString),
          Gen.posNum[Int]
        ).mapN(AvroF.fixed[T])
      )
    )
  }

  def protobufFMessage[T](implicit T: Arbitrary[T]): Gen[ProtobufF.TMessage[T]] = {

    val sampleField: Gen[FieldF.Field[T]] = {
      for {
        name     <- nonEmptyString
        tpe      <- T.arbitrary
        position <- smallNumber
      } yield FieldF.Field(name, tpe, position, List(), false, isMapField = false)
    }

    for {
      name           <- nonEmptyString
      field          <- sampleField
      nestedMessages <- Gen.listOfN(1, T.arbitrary)
      nestedEnums    <- Gen.listOfN(1, T.arbitrary)
    } yield ProtobufF.TMessage(name, List(field), Nil, nestedMessages, nestedEnums)
  }

  val genOption: Gen[ProtobufF.OptionValue] = (nonEmptyString, nonEmptyString).mapN(ProtobufF.OptionValue.apply)

  def protobufFEnum[T]: Gen[ProtobufF.TEnum[T]] =
    (
      nonEmptyString,
      Gen.listOf((nonEmptyString, Gen.posNum[Int]).tupled),
      Gen.listOf(genOption),
      Gen.listOf((nonEmptyString, Gen.posNum[Int]).tupled)
    ).mapN(ProtobufF.TEnum[T])

  implicit def protoArbitrary[T](implicit T: Arbitrary[T]): Arbitrary[ProtobufF[T]] = {
    Arbitrary(
      Gen.oneOf(
        ProtobufF.double[T]().pure[Gen],
        ProtobufF.float[T]().pure[Gen],
        ProtobufF.int32[T]().pure[Gen],
        ProtobufF.int64[T]().pure[Gen],
        ProtobufF.uint32[T]().pure[Gen],
        ProtobufF.uint64[T]().pure[Gen],
        ProtobufF.sint32[T]().pure[Gen],
        ProtobufF.sint64[T]().pure[Gen],
        ProtobufF.fixed32[T]().pure[Gen],
        ProtobufF.fixed64[T]().pure[Gen],
        ProtobufF.sfixed32[T]().pure[Gen],
        ProtobufF.sfixed64[T]().pure[Gen],
        ProtobufF.bool[T]().pure[Gen],
        ProtobufF.string[T]().pure[Gen],
        ProtobufF.bytes[T]().pure[Gen],
        (Gen.listOf(nonEmptyString), nonEmptyString) mapN ProtobufF.namedType[T],
        T.arbitrary map ProtobufF.repeated[T],
        protobufFEnum[T],
        protobufFMessage[T]
      )
    )
  }

  def muCoproductWithTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = true).arbitrary

  def muCoproductWithoutTNullGen[T](implicit B: Basis[MuF, T]): Gen[MuF.TCoproduct[T]] =
    muCoproductArbitrary(withTNull = false).arbitrary
}
