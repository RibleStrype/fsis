package fsis

import simulacrum._

@typeclass trait Functor[F[_]] {
  self =>

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  def as[A, B](fa: F[A], b: => B): F[B] =
    map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] =
    as(fa, ())

  def compose[G[_]](implicit G: Functor[G]): Functor[Lambda[X => F[G[X]]]] =
    new Functor[Lambda[X => F[G[X]]]] {
      override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
        self.map(fga)(ga => G.map(ga)(f))
    }
}

object Functor {

  implicit val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

  implicit val optionFunctor = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }

  implicit def function1Functor[T] = new Functor[T => ?] {
    override def map[A, B](fa: T => A)(f: A => B): T => B = fa andThen f
  }
}

trait FunctorLaws[F[_]] {

  import Functor.ops._
  import IsEq._

  implicit def F: Functor[F]

  def identity[A](fa: F[A]) =
    fa.map(a => a) =?= fa

  def composition[A, B, C](fa: F[A], f: A => B, g: B => C) =
    fa.map(f).map(g) =?= fa.map(f andThen g)
}

object FunctorLaws {
  def apply[F[_]](implicit F0: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
    override implicit def F: Functor[F] = F0
  }
}