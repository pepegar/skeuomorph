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

package higherkindness.skeuomorph.protobuf

import higherkindness.skeuomorph.avro.AvroF
import higherkindness.skeuomorph.protobuf.ProtobufF.{
  TBool,
  TBytes,
  TDouble,
  TEnum,
  TFloat,
  TInt32,
  TInt64,
  TMap,
  TMessage,
  TNamedType,
  TNull,
  TOneOf,
  TRepeated,
  TString
}
import qq.droste.Trans

object Transform {

  def tranformAvro[A](): Trans[AvroF, ProtobufF, A] = Trans {
    case AvroF.TNull()          => TNull()
    case AvroF.TBoolean()       => TBool()
    case AvroF.TInt()           => TInt32()
    case AvroF.TLong()          => TInt64()
    case AvroF.TFloat()         => TFloat()
    case AvroF.TDouble()        => TDouble()
    case AvroF.TBytes()         => TBytes()
    case AvroF.TString()        => TString()
    case AvroF.TNamedType(name) => TNamedType(name)
    case AvroF.TArray(value)    => TRepeated(value)
    case AvroF.TMap(values)     => TMap(values, values)
    case AvroF.TRecord(name, _, _, _, fields) =>
      TMessage(name, fields.zipWithIndex.map {
        case (f, pos) => FieldF.Field(f.name, f.tpe, pos, List.empty, isRepeated = false, isMapField = false)
      }, List.empty)
    case AvroF.TEnum(name, _, aliases, _, symbols) =>
      TEnum(name, symbols.zipWithIndex, List.empty, aliases.zipWithIndex)
    case AvroF.TUnion(options) =>
      TOneOf("", options.zipWithIndex.map {
        case (a, pos) => FieldF.Field("", a, pos, List.empty, isRepeated = false, isMapField = false)
      })
    case AvroF.TFixed(_, _, _, _) => ???
  }
}
