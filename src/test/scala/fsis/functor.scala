package fsis

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

abstract class FunctorInstanceTest[F[_]](name: String)(implicit
  F: Functor[F],
  arbInt: Arbitrary[F[Int]],
  eqInt: Equal[F[Int]],
  eqLong: Equal[F[Long]]
) extends Properties(s"Functor[$name]") {

  val laws = FunctorLaws[F]

  property("identity") = forAll { a: F[Int] =>
    laws.identity(a).isEqual
  }

  property("composition") = forAll { (a: F[Int], f: Int => String, g: String => Long) =>
    laws.composition(a, f, g).isEqual
  }
}

class ListFunctorTest extends FunctorInstanceTest[List]("List")

class OptionFunctorTest extends FunctorInstanceTest[Option]("Option")