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

import cats.data.NonEmptyList
import cats.syntax.option._
import higherkindness.skeuomorph.{mu, UnsupportedRequestTypeException, UnsupportedResponseTypeException}
import higherkindness.skeuomorph.mu.{MuF, SerializationType}
import io.circe.Json
import org.apache.avro.{Schema, Protocol => AvroProtocol}
import higherkindness.droste._
import higherkindness.droste.syntax.all._
import org.apache.avro.Schema.Type

import scala.jdk.CollectionConverters._

final case class Protocol[A](
    name: String,
    namespace: Option[String],
    types: List[A],
    messages: List[Protocol.Message[A]]
)

object Protocol {
  import AvroF._

  final case class Message[A](name: String, request: A, response: A)

  object Message {
    def toJson[T](message: Message[T])(implicit T: Project[AvroF, T]): Json = {
      val avroToJson = scheme.cata(AvroF.toJson)

      Json.obj(
        "request" -> Json.arr(
          Json.obj(
            "name" -> Json.fromString("arg"), //TODO: is this doable?
            "type" -> avroToJson(message.request)
          )
        ),
        "response" -> avroToJson(message.response)
      )
    }
  }

  def fromProto[T](proto: AvroProtocol)(implicit T: Embed[AvroF, T]): Protocol[T] = {
    val toAvroF: Schema => T = scheme.ana(fromAvro)

    def requestToAvroF(req: Schema): AvroF[T] = {
      if (req.getType == Schema.Type.NULL)
        `null`[T]()
      else {
        val fields = req.getFields
        if (fields.size == 0)
          `null`[T]()
        else {
          // Assume it's a record type.
          // We don't support primitive types for RPC requests/responses.
          val fieldSchema = fields.get(0).schema
          fieldSchema.getType match {
            case Type.RECORD => namedType[T](fieldSchema.getNamespace, fieldSchema.getName)
            case nonRecord =>
              throw UnsupportedRequestTypeException(
                s"Skeuomorph only supports Record types for Avro requests. Encountered request schema with type $nonRecord"
              )
          }
        }
      }
    }

    def responseToAvroF(resp: Schema): AvroF[T] = {
      if (resp.getType == Schema.Type.NULL)
        `null`[T]()
      else
        // Assume it's a record type.
        // We don't support primitive types for RPC requests/responses.
        resp.getType match {
          case Type.RECORD => namedType[T](resp.getNamespace, resp.getName)
          case nonRecord =>
            throw UnsupportedResponseTypeException(
              s"Skeuomorph only supports Record types for Avro responses. Encountered response schema with type $nonRecord"
            )
        }
    }

    def toMessage(kv: (String, AvroProtocol#Message)): Message[T] = {
      Message[T](
        kv._2.getName,
        requestToAvroF(kv._2.getRequest).embed,
        responseToAvroF(kv._2.getResponse).embed
      )
    }
    Protocol(
      proto.getName,
      Option(proto.getNamespace),
      proto.getTypes.asScala.toList.map(toAvroF),
      proto.getMessages.asScala.toList.map(toMessage)
    )
  }

  def toJson[T](proto: Protocol[T])(implicit T: Basis[AvroF, T]): Json = {
    val withNamespace: Json = Json.fromFields(proto.namespace.map("namespace" -> Json.fromString(_)).toList)

    withNamespace deepMerge Json.obj(
      "protocol" -> Json.fromString(proto.name),
      "types"    -> Json.fromValues(proto.types.map(scheme.cata(AvroF.toJson))),
      "messages" -> Json.fromFields(
        proto.messages.map(m => m.name -> Message.toJson(m))
      )
    )
  }

  def fromMuSchema[T](implicit T: Basis[AvroF, T]): Trans[MuF, AvroF, T] =
    Trans {
      case MuF.TNull()                                => AvroF.`null`()
      case MuF.TDouble()                              => AvroF.double()
      case MuF.TFloat()                               => AvroF.float()
      case MuF.TInt(MuF._32)                          => AvroF.int()
      case MuF.TInt(MuF._64)                          => AvroF.long()
      case MuF.TBoolean()                             => AvroF.boolean()
      case MuF.TString()                              => AvroF.string()
      case MuF.TByteArray(MuF.Length.Arbitrary)       => AvroF.bytes()
      case MuF.TByteArray(MuF.Length.Fixed(n, ns, l)) => AvroF.fixed(n, ns, Nil, l)
      case MuF.TNamedType(prefix, name)               => AvroF.namedType(prefix.mkString("."), name)
      case MuF.TOption(value)                         => AvroF.union(NonEmptyList(AvroF.`null`[T]().embed, List(value)))
      case MuF.TEither(left, right)                   => AvroF.union(NonEmptyList(left, List(right)))
      case MuF.TList(value)                           => AvroF.array(value)
      case MuF.TMap(_, value)                         => AvroF.map(value)
      case MuF.TGeneric(_, _)                         => ??? // WAT
      case MuF.TContaining(_)                         => ??? // TBD
      case MuF.TRequired(t)                           => T.coalgebra(t)
      case MuF.TCoproduct(invariants)                 => AvroF.union(invariants)
      case MuF.TSum(name, fields)                     => AvroF.enum(name, none[String], Nil, none[String], fields.map(_.name))
      case MuF.TProduct(name, namespace, fields, _, _) =>
        TRecord(
          name,
          namespace,
          Nil,
          none[String],
          fields.map(f => Field(f.name, Nil, none[String], none[Order], f.tpe))
        )
    }

  def fromMuProtocol[T, U](protocol: mu.Protocol[T])(implicit T: Basis[MuF, T], U: Basis[AvroF, U]): Protocol[U] = {
    def fromMu: T => U = scheme.cata(fromMuSchema.algebra)
    val services: List[Message[U]] = protocol.services
      .filter(_.serializationType == SerializationType.Avro)
      .flatMap(s => s.operations.map(op => Message(op.name, fromMu(op.request.tpe), fromMu(op.response.tpe))))

    Protocol(
      protocol.name.getOrElse("Protocol"),
      protocol.pkg,
      protocol.declarations.map(fromMu),
      services
    )
  }
}
