/*
 * Copyright 2018 47 Degrees, LLC. <http://www.47deg.com>
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

package skeuomorph
package protobuf

import org.typelevel.discipline.specs2.Discipline
import cats.laws.discipline.{FoldableTests, FunctorTests, TraverseTests}
import cats.implicits._
import org.specs2._
import instances._

class ProtoSchemaSpec extends Specification with ScalaCheck with Discipline {

  def is = s2"""
  $traverse
  $functor
  $foldable
  """

  val traverse =
    checkAll("Traverse[ProtobufF]", TraverseTests[ProtobufF].traverse[Int, Int, Int, Set[Int], Option, Option])
  val functor  = checkAll("Functor[ProtobufF]", FunctorTests[ProtobufF].functor[Int, Int, String])
  val foldable = checkAll("Foldable[ProtobufF]", FoldableTests[ProtobufF].foldable[Int, Int])
}
