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

package higherkindness.skeuomorph

import catz.contrib.Decidable
import cats.{Contravariant, Show}
import cats.syntax.all._
import cats.kernel.Eq

trait Printer[T] extends Printer.ContravariantPrinter[T]

object Printer {

  def apply[A](implicit instance: Printer[A]): Printer[A] = instance

  /**
   * creates an instance of [[Printer]] using the provided function
   */
  def print[A](f: A => String): Printer[A] =
    new Printer[A] {
      def print(a: A): String = f(a)
    }

  trait ContravariantPrinter[-T] extends Serializable {
    def print(t: T): String
  }

  trait Ops[A] {
    def typeClassInstance: Printer[A]
    def self: A
    def print: String = typeClassInstance.print(self)
  }

  trait ToPrinterOps {
    implicit def toPrinter[A](target: A)(implicit tc: Printer[A]): Ops[A] =
      new Ops[A] {
        val self              = target
        val typeClassInstance = tc
      }
  }

  def konst(str: String): Printer[Unit] =
    Printer(Function.const(str)(_))

  val space: Printer[Unit] = konst(" ")

  val newLine: Printer[Unit] = konst("\n")

  val string: Printer[String] = print(identity)

  val identifier: Printer[String] = print(toValidIdentifier)

  val unit: Printer[Unit] = print(_ => "")

  object avoid {
    implicit def nonePrinter[T]: Printer[T] = Contravariant[Printer].contramap(unit)(_ => ())
  }

  /**
   * creates an instance of [[Printer]] using object toString
   */
  def fromToString[A]: Printer[A] =
    new Printer[A] {
      def print(a: A): String = a.toString
    }

  def show[F: Show]: Printer[F] = Printer(_.show)

  def optional[A](p: Printer[A]): Printer[Option[A]] =
    Printer(_.fold("")(p.print))

  def sepBy[A](p: Printer[A], sep: String): Printer[List[A]] =
    print {
      case Nil => ""
      case xs  => xs.map(p.print).mkString(sep)
    }

  /*
   * The logic to decide whether a given string needs to be wrapped in backticks
   * to be a valid Scala identifier is really complicated (the spec is at
   * https://scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html#identifiers,
   * but it's quite wrong/misleading). So we let scalameta take care of it for us.
   */
  def toValidIdentifier(string: String): String =
    if (string.isEmpty) string else scala.meta.Term.Name(string).syntax

  implicit val divisiblePrinter: Decidable[Printer] = new Decidable[Printer] {
    def unit: Printer[Unit] = Printer.unit
    def product[A, B](fa: Printer[A], fb: Printer[B]): Printer[(A, B)] =
      new Printer[(A, B)] {
        def print(ab: (A, B)): String = fa.print(ab._1) + fb.print(ab._2)
      }
    def contramap[A, B](fa: Printer[A])(f: B => A): Printer[B] =
      print[B]((fa.print _).compose(f))
    def choose[A, B, C](fa: Printer[A], fb: Printer[B])(cab: C => Either[A, B]): Printer[C] =
      Printer(cab(_).fold(fa.print, fb.print))
  }

  implicit val catsContravariantForPrinter: Contravariant[Printer] = new Contravariant[Printer] {
    def contramap[A, B](fa: Printer[A])(f: B => A): Printer[B] =
      print[B]((fa.print _).compose(f))
  }

  implicit def catsLawsEqForPrinter[A](implicit ev: Eq[A => String]): Eq[Printer[A]] =
    Eq.by[Printer[A], A => String](printA => a => printA.print(a))
}
