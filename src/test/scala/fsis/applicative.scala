package fsis

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

abstract class ApplicativeInstanceTest[F[_]](name: String)(implicit
  F: Applicative[F],
  arbInt: Arbitrary[F[Int]],
  arbIntToString: Arbitrary[F[Int => String]],
  eqInt: Equal[F[Int]],
  eqString: Equal[F[String]]
) extends Properties(s"Applicative[$name]") {

  val laws = ApplicativeLaws[F]

  property("applicative identity") = forAll { a: F[Int] =>
    laws.applicativeIdentity(a).isEqual
  }

  property("applicative homomorphism") = forAll { (a: Int, f: Int => String) =>
    laws.applicativeHomomorphism(a, f).isEqual
  }

  property("applicative interchange") = forAll { (a: Int, f: F[Int => String]) =>
    laws.applicativeInterchange(a, f).isEqual
  }

  property("applicative map") = forAll { (a: F[Int], f: Int => String) =>
    laws.applicativeMap(a, f).isEqual
  }
}

class ListApplicativeTest extends ApplicativeInstanceTest[List]("List")

class OptionApplicativeTest extends ApplicativeInstanceTest[Option]("Option")