package fsis

import simulacrum._

@typeclass trait Applicative[F[_]] extends Functor[F] {
  self =>

  def pure[A](a: A): F[A]

  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  def apply2[A, B, Z](fa: F[A], fb: F[B])(ff: F[(A, B) => Z]): F[Z] =
    map3(fa, fb, ff)((a, b, f) => f(a, b))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
    apply(fa)(map2(fb, fc)((b, c) => f(_, b, c)))

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) {
      case ((a, b), (c, d)) => f(a, b, c, d)
    }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((_, _, _))

  def flip[A, B](ff: F[A => B]): F[A] => F[B] =
    fa => apply(fa)(ff)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[Lambda[X => F[G[X]]]] =
    new Applicative[Lambda[X => F[G[X]]]] {
      override def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

      override def apply[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] =
        self.apply(fga)(self.map(ff)(G.flip))
    }
}

object Applicative {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

    override def pure[A](a: A): Option[A] = Some(a)

    override def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] =
      (fa, ff) match {
        case (Some(a), Some(f)) => Some(f(a))
        case _                  => None
      }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {

    override def pure[A](a: A): List[A] = a :: Nil

    override def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] =
      for {
        a <- fa
        f <- ff
      } yield f(a)
  }

}

trait ApplicativeLaws[F[_]] {

  import Applicative.ops._
  import IsEq._

  implicit def F: Applicative[F]

  def applicativeIdentity[A](fa: F[A]) =
    fa.apply(F.pure((a: A) => a)) =?= fa

  def applicativeHomomorphism[A, B](a: A, f: A => B) =
    F.pure(a).apply(F.pure(f)) =?= F.pure(f(a))

  def applicativeInterchange[A, B](a: A, ff: F[A => B]) =
    F.pure(a).apply(ff) =?= ff.apply(F.pure((f: A => B) => f(a)))

  def applicativeMap[A, B](fa: F[A], f: A => B) =
    fa.map(f) =?= fa.apply(F.pure(f))
}

object ApplicativeLaws {
  def apply[F[_]](implicit F0: Applicative[F]): ApplicativeLaws[F] = new ApplicativeLaws[F] {
    override implicit def F: Applicative[F] = F0
  }
}