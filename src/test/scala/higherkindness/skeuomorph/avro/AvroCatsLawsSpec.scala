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

import higherkindness.skeuomorph.instances._
import org.typelevel.discipline.specs2.Discipline
import cats.laws.discipline.{FoldableTests, FunctorTests, TraverseTests}
import org.specs2._

class AvroCatsLawsSpec extends Specification with ScalaCheck with Discipline {

  def is = s2"""
  $traverse
  $functor
  $foldable
  """

  val traverse = checkAll("Traverse[AvroF]", TraverseTests[AvroF].traverse[Int, Int, Int, Set[Int], Option, Option])
  val functor  = checkAll("Functor[AvroF]", FunctorTests[AvroF].functor[Int, Int, String])
  val foldable = checkAll("Foldable[AvroF]", FoldableTests[AvroF].foldable[Int, Int])
}
