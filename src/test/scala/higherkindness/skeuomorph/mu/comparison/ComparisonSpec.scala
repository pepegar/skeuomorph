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

package higherkindness.skeuomorph.mu.comparison

import higherkindness.droste.data.Mu
import higherkindness.droste.syntax.embed._
import org.specs2.Specification
import cats.data.NonEmptyList

import higherkindness.skeuomorph.mu.MuF, MuF._
import Comparison.Match
import Transformation._
import PathElement._

class ComparisonSpec extends Specification {

  def is =
    s2"""
  Schema comparison

  Should accept numeric widening $numericWidening
  Should accept coproduct creation $coproductCreation
  Should accept coproduct widening $coproductWidening
  Should accept field addition in records $fieldAddition
  Should accept field addition in nested records $fieldAdditionNestedProduct
  Should accept field removal in records $fieldRemoval
  Should accept making a type optional $optionalPromotion
  Should accept promotion to either $eitherPromotion
  Should accept promotion of option to either $optionToEither
  Should accept promotion of option to coproduct $optionToCoproduct
  Should accept promotion of either to coproduct $eitherToCoproduct
  """

  type T = Mu[MuF]

  def numericWidening = {

    val validWidenings = List(
      int[T]().embed   -> List(long[T]().embed, float[T]().embed, double[T]().embed),
      long[T]().embed  -> List(float[T]().embed, double[T]().embed),
      float[T]().embed -> List(double[T]().embed)
    )

    for {
      (w, rs) <- validWidenings
      r       <- rs
    } yield Comparison(w, r) must_== Match(Path.empty, NumericWidening(w, r))
  }

  def coproductCreation = {
    val original = int[T]().embed
    val extended = coproduct(NonEmptyList.of(string[T]().embed, long[T]().embed)).embed

    Comparison(original, extended) must_== Match(
      Path.empty -> List(
        PromotionToCoproduct(extended),
        NumericWidening(original, long[T]().embed)
      )
    )
  }

  def optionToEither = {
    val original = option(int[T]().embed).embed
    val extended = either(int[T]().embed, string[T]().embed).embed

    Comparison(original, extended) must_== Match(Path.empty, PromotionToEither(extended))

  }

  def optionToCoproduct = {
    val original = option(int[T]().embed).embed
    val extended = coproduct(NonEmptyList.of(int[T]().embed, string[T]().embed, `null`[T]().embed)).embed

    Comparison(original, extended) must_== Match(Path.empty, PromotionToCoproduct(extended))
  }

  def eitherToCoproduct = {
    val original = either(int[T]().embed, string[T]().embed).embed
    val extended = coproduct(
      NonEmptyList.of(byteArray[T](Length.Arbitrary).embed, boolean[T]().embed, long[T]().embed)
    ).embed

    Comparison(original, extended) must_== Match(
      Path.empty               -> List(PromotionToCoproduct(extended)),
      Path.empty / LeftBranch  -> List(NumericWidening(int[T]().embed, long[T]().embed)),
      Path.empty / RightBranch -> List(StringConversion(string[T]().embed, byteArray[T](Length.Arbitrary).embed))
    )
  }

  def coproductWidening = {
    val original = coproduct(NonEmptyList.of(string[T]().embed, long[T]().embed)).embed
    val extended = coproduct(
      NonEmptyList.of(float[T]().embed, byteArray[T](Length.Arbitrary).embed, boolean[T]().embed)
    ).embed

    Comparison(original, extended) must_== Match(
      Path.empty / Alternative(0) -> List(StringConversion(string[T]().embed, byteArray[T](Length.Arbitrary).embed)),
      Path.empty / Alternative(1) -> List(NumericWidening(long[T]().embed, float[T]().embed))
    )
  }

  def fieldAddition = {
    val original = product("foo", Some("bar"), List(Field("name", string[T]().embed, Some(List(1)))), Nil, Nil).embed
    val extended =
      product(
        "foo",
        Some("bar"),
        List(Field("name", string[T]().embed, Some(List(1))), Field("age", int[T]().embed, Some(List(2)))),
        Nil,
        Nil
      ).embed

    Comparison(original, extended) must_== Match(Path.empty / Name("foo") / FieldName("age"), Addition(int[T]().embed))
  }

  def fieldRemoval = {
    val original =
      product(
        "foo",
        Some("bar"),
        List(Field("name", string[T]().embed, Some(List(1))), Field("age", int[T]().embed, Some(List(2)))),
        Nil,
        Nil
      ).embed
    val reduced = product("foo", Some("bar"), List(Field("name", string[T]().embed, Some(List(1)))), Nil, Nil).embed

    Comparison(original, reduced) must_== Match(Path.empty / Name("foo") / FieldName("age"), Removal(int[T]().embed))
  }

  def optionalPromotion = {

    val original =
      product(
        "foo",
        Some("bar"),
        List(Field("name", string[T]().embed, Some(List(1))), Field("age", int[T]().embed, Some(List(2)))),
        Nil,
        Nil
      ).embed
    val promoted = product(
      "foo",
      Some("bar"),
      List(
        Field("name", string[T]().embed, Some(List(1))),
        Field("age", option(int[T]().embed).embed, Some(List(2)))
      ),
      Nil,
      Nil
    ).embed

    Comparison(original, promoted) must_== Match(Path.empty / Name("foo") / FieldName("age"), PromotionToOption[T]())
  }

  def eitherPromotion = {

    val original =
      product(
        "foo",
        Some("bar"),
        List(Field("name", string[T]().embed, Some(List(1))), Field("age", int[T]().embed, Some(List(2)))),
        Nil,
        Nil
      ).embed
    val promotedL = product(
      "foo",
      Some("bar"),
      List(
        Field("name", string[T]().embed, Some(List(1))),
        Field("age", either(int[T]().embed, boolean[T]().embed).embed, Some(List(2)))
      ),
      Nil,
      Nil
    ).embed
    val promotedR = product(
      "foo",
      Some("bar"),
      List(
        Field("name", either(long[T]().embed, string[T]().embed).embed, Some(List(1))),
        Field("age", int[T]().embed, Some(List(2)))
      ),
      Nil,
      Nil
    ).embed

    Comparison(original, promotedL) must_== Match(
      Path.empty / Name("foo") / FieldName("age"),
      PromotionToEither(either(int[T]().embed, boolean[T]().embed).embed)
    )
    Comparison(original, promotedR) must_== Match(
      Path.empty / Name("foo") / FieldName("name"),
      PromotionToEither(either(long[T]().embed, string[T]().embed).embed)
    )

  }

  def fieldAdditionNestedProduct = {
    val original = product(
      "foo",
      Some("bar"),
      List(Field("name", string[T]().embed, Some(List(1)))),
      List(product("bar", Some("foo"), List(Field("first", string[T]().embed, Some(List(1)))), Nil, Nil).embed),
      Nil
    ).embed
    val extended = product(
      "foo",
      Some("bar"),
      List(Field("name", string[T]().embed, Some(List(1)))),
      List(
        product(
          "bar",
          Some("foo"),
          List(Field("first", string[T]().embed, Some(List(1))), Field("second", int[T]().embed, Some(List(1)))),
          Nil,
          Nil
        ).embed
      ),
      Nil
    ).embed

    Comparison(original, extended) must_== Match(
      Path.empty / Name("foo") / Name("bar") / FieldName("second"),
      Addition(int[T]().embed)
    )
  }
}
