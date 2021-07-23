/*
 * Copyright 2018-2019 47 Degrees, LLC. <http://www.47deg.com>
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

import higherkindness.skeuomorph.avro.AvroF.{
  Field,
  TArray,
  TBoolean,
  TBytes,
  TDouble,
  TEnum,
  TFloat,
  TInt,
  TLong,
  TMap,
  TNamedType,
  TNull,
  TRecord,
  TString,
  TUnion
}
import higherkindness.skeuomorph.protobuf.ProtobufF
import qq.droste.Trans

object Transform {

  def transformProto[A](): Trans[ProtobufF, AvroF, A] = Trans {
    case ProtobufF.TNull()          => TNull()
    case ProtobufF.TDouble()        => TDouble()
    case ProtobufF.TFloat()         => TFloat()
    case ProtobufF.TInt32()         => TInt()
    case ProtobufF.TInt64()         => TLong()
    case ProtobufF.TUint32()        => TInt()
    case ProtobufF.TUint64()        => TLong()
    case ProtobufF.TSint32()        => TInt()
    case ProtobufF.TSint64()        => TLong()
    case ProtobufF.TFixed32()       => TInt()
    case ProtobufF.TFixed64()       => TLong()
    case ProtobufF.TSfixed32()      => TInt()
    case ProtobufF.TSfixed64()      => TLong()
    case ProtobufF.TBool()          => TBoolean()
    case ProtobufF.TString()        => TString()
    case ProtobufF.TBytes()         => TBytes()
    case ProtobufF.TNamedType(name) => TNamedType(name)
    case ProtobufF.TRepeated(value) => TArray(value)
    case ProtobufF.TEnum(name, symbols, _, aliases) =>
      TEnum(name, None, aliases.map { case (alias, _) => alias }, None, symbols.map { case (symbol, _) => symbol })
    case ProtobufF.TMessage(name, fields, _) =>
      TRecord(name, None, List.empty, None, fields.map(f => Field(f.name, List.empty, None, None, f.tpe)))
    case ProtobufF.TFileDescriptor(values, name, pkg) =>
      TRecord(name, Some(pkg), List.empty, None, values.map(tpe => Field("", List.empty, None, None, tpe)))
    case ProtobufF.TOneOf(_, fields) => TUnion(fields.map(_.tpe))
    case ProtobufF.TMap(_, values)   => TMap(values)
  }

}
