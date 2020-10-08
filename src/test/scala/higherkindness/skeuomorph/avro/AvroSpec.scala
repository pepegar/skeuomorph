/*
 * Copyright 2018-2020 47 Degrees Open Source <https://www.47deg.com>
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

import java.io.File

import avrohugger.format.Standard
import avrohugger.input.parsers.{FileInputParser, IdlImportParser}
import avrohugger.stores.ClassStore
import higherkindness.skeuomorph.instances._
import org.apache.avro.Schema
import org.apache.avro.compiler.idl._
import org.scalacheck._
import org.specs2._
import higherkindness.skeuomorph.mu.{CompressionType, MuF, codegen, Protocol => MuProtocol}
import higherkindness.droste._
import higherkindness.droste.data.Mu

import scala.meta._
import scala.meta.contrib._
import scala.collection.JavaConverters._
import scala.util.Try

class AvroSpec extends Specification with ScalaCheck {

  def is = s2"""
  Avro Schema

  It should be possible to create a Schema from org.apache.avro.Schema. $convertSchema

  It should be possible to create a Protocol from org.apache.avro.Protocol and then generate Scala code from it. $checkAllValid

  It should generate an error message when encountering invalid avro definition. $checkAllInvalid
  """

  def convertSchema =
    Prop.forAll { (schema: Schema) =>
      val test = scheme.hylo(checkSchema(schema), AvroF.fromAvro)

      test(schema)
    }

  def checkSchema(sch: Schema): Algebra[AvroF, Boolean] =
    Algebra {
      case AvroF.TNull()    => sch.getType should_== Schema.Type.NULL
      case AvroF.TBoolean() => sch.getType should_== Schema.Type.BOOLEAN
      case AvroF.TInt()     => sch.getType should_== Schema.Type.INT
      case AvroF.TLong()    => sch.getType should_== Schema.Type.LONG
      case AvroF.TFloat()   => sch.getType should_== Schema.Type.FLOAT
      case AvroF.TDouble()  => sch.getType should_== Schema.Type.DOUBLE
      case AvroF.TBytes()   => sch.getType should_== Schema.Type.BYTES
      case AvroF.TString()  => sch.getType should_== Schema.Type.STRING

      case AvroF.TNamedType(_, _) => false
      case AvroF.TArray(_)        => sch.getType should_== Schema.Type.ARRAY
      case AvroF.TMap(_)          => sch.getType should_== Schema.Type.MAP
      case AvroF.TRecord(name, namespace, _, doc, fields) =>
        (sch.getName should_== name)
          .and(sch.getNamespace should_== namespace.getOrElse(""))
          .and(sch.getDoc should_== doc.getOrElse(""))
          .and(
            sch.getFields.asScala.toList.map(f => (f.name, f.doc)) should_== fields
              .map(f => (f.name, f.doc.getOrElse("")))
          )

      case AvroF.TEnum(_, _, _, _, _) => true
      case AvroF.TUnion(_)            => true
      case AvroF.TFixed(_, _, _, _)   => true
    }

  def checkAllValid =
    List(
      "MyGreeterService",
      "LogicalTypes",
      "NestedRecords",
      "ImportedService",
      "Fixed"
    ).map(checkValid)

  def checkValid(idlName: String) = {
    val actual = gen(idlName).fold(sys.error, identity)
    val expected = codegenExpectation(idlName, Some(CompressionType.Identity), true)
      .parse[Source]
      .get
      .children
      .head
      .asInstanceOf[Pkg]
    actual.isEqual(expected) must beTrue setMessage (
      s"""
      |Actual output:
      |$actual
      |
      |
      |Expected output:
      |$expected
      """.stripMargin
    )
  }

  def checkAllInvalid =
    List("InvalidRequest", "InvalidResponse").map(checkInvalid)

  def checkInvalid(idlName: String) =
    gen(idlName) must beLeft

  private def gen(idlName: String) = {
    val idlResourceName = s"avro/${idlName}.avdl"
    val idlUri = Try(getClass.getClassLoader.getResource(idlResourceName).toURI)
      .fold(t => sys.error(s"Unable to get resource $idlResourceName due to ${t.getMessage}"), identity)
    val idlFile = new File(idlUri)

    def avroProtos =
      (new FileInputParser).getSchemaOrProtocols(idlFile, Standard, new ClassStore, getClass.getClassLoader).collect {
        case Right(protocol) => protocol
      }
    val avroProto = avroProtos
      .find(p => p.getName == idlName)
      .getOrElse(sys.error(s"No protocol found for name ${idlName} in ${avroProtos.map(_.getName)}"))

    val skeuoAvroProto = Try(Protocol.fromProto[Mu[AvroF]](avroProto)).toEither.left.map(_.getMessage)

    val muProto = skeuoAvroProto.map { p =>
      MuProtocol.fromAvroProtocol(CompressionType.Identity, useIdiomaticEndpoints = true)(p)
    }

    val streamCtor: (Type, Type) => Type.Apply = { case (f: Type, a: Type) =>
      t"Stream[$f, $a]"
    }

    muProto.flatMap(p => codegen.protocol(p, streamCtor))
  }

  // TODO test for more complex schemas, importing other files, etc.

  def codegenExpectation(
      idlName: String,
      compressionType: Option[CompressionType] = None,
      useIdiomaticEndpoints: Boolean = false
  ): String = {
    val compressionName = compressionType match {
      case Some(CompressionType.Identity) => "Identity"
      case Some(CompressionType.Gzip)     => "Gzip"
      case None                           => ""
    }
    val idiomaticName = if (useIdiomaticEndpoints) "Idiomatic" else ""
    val resourceName  = s"avro/${idlName}${compressionName}${idiomaticName}.scala"
    scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(resourceName)).getLines.mkString("\n")
  }

}
