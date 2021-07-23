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

import java.io.File

import avrohugger.format.Standard
import avrohugger.input.parsers.FileInputParser
import avrohugger.stores.ClassStore
import higherkindness.droste.data.Mu
import higherkindness.skeuomorph.mu.{CompressionType, SerializationType, codegen, Protocol => MuProtocol}
import org.fusesource.scalate.{TemplateEngine, TemplateSource}
import org.scalacheck._
import org.specs2._

import scala.meta._
import scala.meta.contrib._
import scala.util.Try

class AvroProtocolSpec extends Specification with ScalaCheck {

  implicit val arbCompressionType: Arbitrary[CompressionType] = Arbitrary {
    Gen.oneOf(CompressionType.Identity, CompressionType.Gzip)
  }

  case class ValidAvdlName(name: String)
  implicit val validAvdl: Arbitrary[ValidAvdlName] = Arbitrary {
    Gen
      .oneOf(
        "MyGreeterService",
        "LogicalTypes",
        "NestedRecords",
        "ImportedService",
        "Fixed",
        "Primitives",
        "Complex"
      )
      .map(ValidAvdlName)
  }

  case class InvalidAvdlName(name: String)
  implicit val invalidAvdl: Arbitrary[InvalidAvdlName] = Arbitrary {
    Gen.oneOf("InvalidRequest", "InvalidResponse").map(InvalidAvdlName)
  }

  def is = s2"""
  Avro Protocol

  It should be possible to create a Protocol from org.apache.avro.Protocol and then generate Scala code from it. $codegenAvroProtocol

  It should generate an error message when encountering invalid avro definition. $checkAllInvalid
  """

  def codegenAvroProtocol =
    prop { (avdlName: ValidAvdlName, compressionType: CompressionType, useIdiomaticEndpoints: Boolean) =>
      val (pkg, actual) = gen(avdlName.name, compressionType, useIdiomaticEndpoints)
        .fold(left => throw new Exception(s"Failed either: $left"), identity)

      val expected = codegenExpectation(avdlName.name, compressionType, pkg, useIdiomaticEndpoints)
        .parse[Source]
        .get
        .children
        .head
        .asInstanceOf[Pkg]

      actual.isEqual(expected) :| s"""
        |Actual output:
        |$actual
        |
        |
        |Expected output:
        |$expected"
        """.stripMargin
    }

  def gen(
      name: String,
      compressionType: CompressionType,
      useIdiomaticEndpoints: Boolean
  ): Either[String, (Option[String], Pkg)] = {
    val idlResourceName = s"avro/$name.avdl"
    val idlUri = Try(getClass.getClassLoader.getResource(idlResourceName).toURI)
      .fold(t => sys.error(s"Unable to get resource $idlResourceName due to ${t.getMessage}"), identity)
    val idlFile = new File(idlUri)

    def avroProto =
      (new FileInputParser)
        .getSchemaOrProtocols(idlFile, Standard, new ClassStore, getClass.getClassLoader)
        .collectFirst {
          case Right(protocol) if protocol.getName == name => protocol
        }
        .getOrElse(sys.error(s"No protocol found for name $name in $idlResourceName"))

    val skeuoAvroProto = Try(Protocol.fromProto[Mu[AvroF]](avroProto)).toEither.left.map(_.getMessage)

    val muProto = skeuoAvroProto.map { p =>
      MuProtocol.fromAvroProtocol(compressionType, useIdiomaticEndpoints)(p)
    }

    val streamCtor: (Type, Type) => Type.Apply = { case (f: Type, a: Type) =>
      t"Stream[$f, $a]"
    }

    muProto.flatMap { mProto =>
      codegen.protocol(mProto, streamCtor).map(p => (mProto.pkg, p))
    }
  }
  val templateEngine: TemplateEngine = new TemplateEngine()

  def codegenExpectation(
      idlName: String,
      compressionType: CompressionType,
      namespace: Option[String],
      useIdiomaticEndpoints: Boolean
  ): String = {

    val serviceParams: String = Seq(
      SerializationType.Avro,
      s"compressionType = $compressionType",
      s"namespace = ${if (useIdiomaticEndpoints) namespace.map("\"" + _ + "\"") else None}"
    ).mkString(", ")
    val resourceName = s"/avro/$idlName.scala.mustache"

    val file =
      new File(getClass.getResource(resourceName).toURI)
    val template = TemplateSource.fromFile(file)
    templateEngine.layout(template, Map("serviceParams" -> serviceParams))
  }

  def checkAllInvalid = prop { (invalid: InvalidAvdlName) =>
    gen(invalid.name, CompressionType.Identity, true) must beLeft
  }

}
