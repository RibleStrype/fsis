package fsis

import simulacrum._

@typeclass trait Monad[F[_]] extends Applicative[F] {
  self =>

  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(fa)(a => map(ff)(f => f(a)))

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  def compose[G[_]](implicit G: Monad[G]): Monad[Lambda[X => F[G[X]]]] = new Monad[Lambda[X => F[G[X]]]] {

    override def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

    override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = ???
  }
}

object Monad {

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }
}

trait MonadLaws[F[_]] {

  import IsEq._
  import Monad.ops._

  implicit def F: Monad[F]

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]) =
    fa.flatMap(f).flatMap(g) =?= fa.flatMap(f.andThen(_.flatMap(g)))

  def leftIdentity[A, B](a: A, f: A => F[B]) =
    F.pure(a).flatMap(f) =?= f(a)

  def rightIdentity[A](fa: F[A]) =
    fa.flatMap(a => F.pure(a)) =?= fa
}

object MonadLaws {
  def apply[F[_]](implicit F0: Monad[F]): MonadLaws[F] = new MonadLaws[F] {
    override implicit def F: Monad[F] = F0
  }
}