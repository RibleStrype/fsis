package fsis

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

abstract class MonadInstanceTest[F[_]](name: String)(implicit
  F: Monad[F],
  arbInt: Arbitrary[F[Int]],
  arbIntToString: Arbitrary[Int => F[String]],
  arbStringToLong: Arbitrary[String => F[Long]],
  eqInt: Equal[F[Int]],
  eqLong: Equal[F[Long]],
  eqString: Equal[F[String]]
) extends Properties(s"Monad[$name]") {

  val laws = MonadLaws[F]

  property("flatMap associativity") = forAll { (fa: F[Int], f: Int => F[String], g: String => F[Long]) =>
    laws.flatMapAssociativity(fa, f, g).isEqual
  }

  property("left identity") = forAll { (a: Int, f: Int => F[String]) =>
    laws.leftIdentity(a, f).isEqual
  }

  property("right identity") = forAll { fa: F[Int] =>
    laws.rightIdentity(fa).isEqual
  }
}

class ListMonadTest extends MonadInstanceTest[List]("List")
class OptionMonadTest extends MonadInstanceTest[Option]("Option")
