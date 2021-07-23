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

import higherkindness.droste._
import higherkindness.droste.syntax.project.toProjectSyntaxOps
import higherkindness.droste.syntax.embed._
import higherkindness.skeuomorph.mu.MuF, MuF._
import cats.Functor
import cats.syntax.all._
import scala.collection.compat.immutable.LazyList
import scala.collection.compat._

/**
 * An ADT representing the "shape" of the computation of the differences between two schemas.
 *
 * Comparing two schemas involves recursively comparing (various combinations of) their
 * respective sub-schemas; each member of the `Comparison` ADT represents one of such
 * combination of recursive comparisons.
 *
 * @tparam T the type of schemas under comparison, typically a fix-point of [[higherkindness.skeuomorph.mu.MuF]], like `Mu[MuF]`
 * @tparam A the type acted upon by this pattern-functor
 */
sealed trait Comparison[T, A]

object Comparison extends ComparisonInstances {

  /**
   * The schemas being compared at a given path of the current comparison
   */
  type Context[T] = (Path, Option[T], Option[T])

  type CompatibleDelta[T] = Map[Path, List[Transformation[T]]]

  type Result[T] = (CompatibleDelta[T], List[Incompatibility])

  object Result {
    def empty[T]: Result[T]                                      = (Map.empty, Nil)
    def mismatch[T](incompatibility: Incompatibility): Result[T] = (Map.empty, List(incompatibility))
    def isMatch[T](result: Result[T]): Boolean                   = result._2.isEmpty

  }

  object Match {
    def apply[T](path: Path, transformation: Transformation[T]): Result[T] = (Map(path -> List(transformation)), Nil)
    def apply[T](entries: (Path, List[Transformation[T]])*): Result[T]     = (Map(entries: _*), Nil)
    def unapply[T](result: Result[T]): Option[CompatibleDelta[T]] =
      if (result._2.isEmpty) Some(result._1) else None
  }

  /**
   * Function for result enrichment
   */
  type Reporter[T] = Result[T] => Result[T]

  /**
   * Early result: there is nothing left to compare
   */
  final case class End[T, A](result: Result[T]) extends Comparison[T, A]

  /**
   * Perform a single recursive comparison and enrich its result
   */
  final case class Compare[T, A](a: A, reporter: Reporter[T] = Reporter.id[T]) extends Comparison[T, A]

  /**
   * Perform two recursive comparisons and combine their results
   */
  final case class CompareBoth[T, A](x: A, y: A) extends Comparison[T, A]

  /**
   * Perform a list of recursive comparisons, combine and then enrich their results
   */
  final case class CompareList[T, A](items: List[A], reporter: Reporter[T] = Reporter.id[T]) extends Comparison[T, A]

  /**
   * Perform a list of recursive comparisons but return the first positive result (or a mismatch if none), enriched
   */
  final case class MatchInList[T, A](attempts: Vector[A], reporter: Reporter[T] = Reporter.id[T])
      extends Comparison[T, A]

  /**
   * Perform lists of recursive comparisons indexed by paths,
   * then select the first positive result for each entry (or a mismatch if none)
   * then combine results of all entries and enrich it.
   */
  final case class AlignUnionMembers[T, A](attempts: Map[Path, List[A]], reporter: Reporter[T] = Reporter.id[T])
      extends Comparison[T, A]

  /**
   * Performs the comparison of two schemas
   *
   * Compares two schemas to verify that messages written using the writer schema are compatible with the reader schema.
   * Both schemas' roots are shallowly compared to unfold a `Comparison[T, *]`, to compare their children or to signal a result.
   * Comparison branches are then folded back by combining their results.
   *
   * WARNING: The current implementation does a lot of "useless" comparisons when it comes to compare coproducts. This is due to
   * the structure of hylo: when we want to align something with a coproduct, we test all the possible combinations algthough
   * we're only interested in finding the first successful one.
   *
   * @tparam T the concrete schema type, must be a higherkindness.droste.Basis over [[higherkindness.skeuomorph.mu.MuF]]
   * @param writer
   * @param reader
   */
  def apply[T](writer: T, reader: T)(implicit ev: Basis[MuF, T], F: Functor[Comparison[T, *]]): Result[T] =
    scheme
      .hylo(Algebra(combineResults[T]), Coalgebra(unfoldComparison[T]))
      .apply((Path.empty, writer.some, reader.some))

  import PathElement._
  import Transformation._
  import Incompatibility._

  private def combineResults[T]: Comparison[T, Result[T]] => Result[T] = {
    case End(res)                  => res
    case Compare(res, rep)         => rep(res)
    case CompareList(results, rep) => rep(results.combineAll)
    case CompareBoth(res1, res2)   => res1 |+| res2
    case AlignUnionMembers(res, rep) =>
      rep(
        res
          .map { case (p, results) =>
            results
              .find(Result.isMatch)
              .getOrElse(Result.mismatch(UnionMemberRemoved(p)))
          }
          .toList
          .combineAll
      )

    case MatchInList(res, rep) =>
      val firstMatch = res.find(Result.isMatch)
      val searchResult =
        firstMatch.getOrElse(Result.mismatch(Different(Path.empty)))
      rep(searchResult)
  }

  private def unfoldComparison[T](implicit basis: Basis[MuF, T]): (Context[T]) => Comparison[T, Context[T]] = {
    case (path, w @ Some(writer), Some(reader)) =>
      (writer.project, reader.project) match {

        // Identities
        case (TNull(), TNull())                                                                             => same
        case (TDouble(), TDouble())                                                                         => same
        case (TFloat(), TFloat())                                                                           => same
        case (TSimpleInt(size1), TSimpleInt(size2)) if size1 === size2                                      => same
        case (TProtobufInt(size1, mods1), TProtobufInt(size2, mods2)) if size1 === size2 && mods1 === mods2 => same
        case (TProtobufInt(_, _), TProtobufInt(_, _))                                                       => End(Result.mismatch(Different(path)))
        case (TBoolean(), TBoolean())                                                                       => same
        case (TString(), TString())                                                                         => same
        case (TByteArray(Length.Arbitrary), TByteArray(Length.Arbitrary))                                   => same
        case (TByteArray(Length.Fixed(n, ns, l)), TByteArray(Length.Fixed(n2, ns2, l2)))
            if n === n2 && ns === ns2 && l === l2 =>
          same
        case (TNamedType(p1, a), TNamedType(p2, b)) if p1 === p2 && a === b => same
        case (TOption(a), TOption(b))                                       => Compare((path, a.some, b.some))
        case (TList(a), TList(b))                                           => Compare((path / Items, a.some, b.some))
        // According to the spec, Avro ignores the keys' schemas when resolving map schemas
        case (TMap(_, a), TMap(_, b))     => Compare((path / Values, a.some, b.some))
        case (TRequired(a), TRequired(b)) => Compare((path, a.some, b.some))
        case (TContaining(a), TContaining(b)) =>
          CompareList(zipLists(path, a, b, Alternative)) // TODO what's the semantics?
        case (TEither(l1, r1), TEither(l2, r2)) =>
          CompareList(List((path / LeftBranch, l1.some, l2.some), (path / RightBranch, r1.some, r2.some)))
        case (TGeneric(g, p), TGeneric(g2, p2)) =>
          CompareList((path / GenericType, g.some, g2.some) :: zipLists(path, p, p2, GenericParameter))
        case (TCoproduct(i), TCoproduct(i2)) =>
          AlignUnionMembers(
            i.zipWithIndex
              .map { case (item, idx) =>
                path / Alternative(idx) -> (List(item.some), i2.toList.map(_.some)).tupled.map(p =>
                  (path / Alternative(idx), p._1, p._2)
                )
              }
              .toList
              .toMap
          )
        case (TSum(n, f), TSum(n2, f2)) if n === n2 && f.forall(f2.toSet) => same
        case (TProduct(n, ns, f, np, nc), TProduct(n2, ns2, f2, np2, nc2)) if n === n2 && ns === ns2 =>
          val fields           = zipFields(path / Name(n), f, f2)
          val nestedProducts   = zipLists(path, np, np2, _ => Name(n))
          val nestedCoproducts = zipLists(path, nc, nc2, _ => Name(n))
          CompareList(fields ++ nestedProducts ++ nestedCoproducts)

        // Numeric widening
        // TODO: this is not valid for Protobuf or Avro (e.g. changing an int to a float would be a breaking change)
        case (TInt(`_32`), TInt(`_64`)) =>
          End(Match(path, NumericWidening(writer, reader)))
        case (_: TInt[_], TFloat() | TDouble()) | (TFloat(), TDouble()) =>
          End(Match(path, NumericWidening(writer, reader)))

        // String and arbitrary Byte arrays are considered compatible
        case (TByteArray(Length.Arbitrary), TString()) | (TString(), TByteArray(Length.Arbitrary)) =>
          End(Match(path, StringConversion(writer, reader)))

        // Promotions
        case (TOption(i1), TCoproduct(is)) =>
          AlignUnionMembers(
            Map(
              path / Value -> is.toList.map(i => (path / Value, i1.some, i.some)),
              path         -> is.toList.map(i => (path, `null`[T]().embed.some, i.some))
            ),
            Reporter.promotedToCoproduct(path, reader)
          )

        case (TOption(i1), TEither(r1, r2)) =>
          MatchInList(
            Vector((path, i1.some, r1.some), (path, i1.some, r2.some)),
            Reporter.promotedToEither(path, reader)
          )

        case (TEither(l1, r1), TCoproduct(rs)) =>
          AlignUnionMembers(
            Map(
              path / LeftBranch  -> rs.toList.map(rr => (path / LeftBranch, l1.some, rr.some)),
              path / RightBranch -> rs.toList.map(rr => (path / RightBranch, r1.some, rr.some))
            ),
            Reporter.promotedToCoproduct(path, reader)
          )

        case (_, TCoproduct(i2)) =>
          MatchInList(i2.toList.toVector.map(i => (path, w, i.some)), Reporter.promotedToCoproduct(path, reader))
        case (_, TOption(t2)) =>
          Compare((path, w, t2.some), Reporter.madeOptional[T](path))
        case (_, TEither(l2, r2)) =>
          MatchInList(Vector((path, w, l2.some), (path, w, r2.some)), Reporter.promotedToEither(path, reader))

        // No compatible transformation found
        case _ => different(path)
      }
    case (path, None, Some(reader)) => End(Match(path, Addition(reader)))
    case (path, Some(writer), None) => End(Match(path, Removal(writer)))
    case (_, None, None)            => same
  }

  private def same[T, A]: Comparison[T, A] = End(Result.empty)

  private def different[T, A](path: Path): Comparison[T, A] = End(Result.mismatch(Different(path)))

  private def zipLists[T](path: Path, l1: List[T], l2: List[T], pathElem: Int => PathElement): List[Context[T]] = {
    val l1s = l1.to(LazyList).map(_.some) ++ LazyList.continually(None)
    val l2s = l2.to(LazyList).map(_.some) ++ LazyList.continually(None)
    l1s.zip(l2s).takeWhile((None, None) != _).toList.zipWithIndex.map { case (p, i) =>
      (path / pathElem(i), p._1, p._2)
    }
  }

  private def zipFields[T](path: Path, l: List[MuF.Field[T]], r: List[MuF.Field[T]]): List[Context[T]] = {
    def toMapEntry(field: MuF.Field[T]): (String, T) = field.name -> field.tpe

    val left  = l.map(toMapEntry).toMap
    val right = r.map(toMapEntry).toMap

    val keys = left.keySet ++ right.keySet

    keys.toList.map(k => (path / FieldName(k), left.get(k), right.get(k)))
  }

}

trait ComparisonInstances {

  implicit def comparisonCatsFunctor[T] =
    new Functor[Comparison[T, *]] {
      def map[A, B](fa: Comparison[T, A])(f: (A) => B): Comparison[T, B] =
        fa match {
          case Comparison.End(res)            => Comparison.End(res)
          case Comparison.Compare(a, rep)     => Comparison.Compare(f(a), rep)
          case Comparison.CompareBoth(x, y)   => Comparison.CompareBoth(f(x), f(y))
          case Comparison.CompareList(i, rep) => Comparison.CompareList(i.map(f), rep)
          case Comparison.MatchInList(a, rep) => Comparison.MatchInList(a.map(f), rep)
          case Comparison.AlignUnionMembers(a, rep) =>
            Comparison.AlignUnionMembers(a.view.mapValues(_.map(f)).toMap, rep)
        }
    }
}

object Reporter {

  import Comparison.{Match, Result}
  import Transformation._

  def id[T]: Result[T] => Result[T] = r => r

  def appendTransformationToMatch[T](path: Path, transfo: Transformation[T]): Result[T] => Result[T] = { res =>
    if (Result.isMatch(res)) Match(path, transfo) |+| res
    else res
  }

  def madeOptional[T](path: Path): Result[T] => Result[T] = appendTransformationToMatch(path, PromotionToOption[T]())

  def promotedToEither[T](path: Path, either: T): Result[T] => Result[T] =
    appendTransformationToMatch(path, PromotionToEither(either))

  def promotedToCoproduct[T](path: Path, coproduct: T): Result[T] => Result[T] =
    appendTransformationToMatch(path, PromotionToCoproduct(coproduct))

}
