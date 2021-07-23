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

import higherkindness.droste._
import higherkindness.skeuomorph.instances._
import org.apache.avro.Schema
import org.scalacheck._
import org.specs2._

import scala.jdk.CollectionConverters._

class AvroSchemaSpec extends Specification with ScalaCheck {

  def is = s2"""
  Avro Schema

  It should be possible to create a Schema from org.apache.avro.Schema. $convertSchema
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
}
