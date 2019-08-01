package codes.quine.labo
package neko
package laws

import syntax._

trait ApplicativeLaws[F[_]] extends FunctorLaws[F] {
  implicit override val F: Applicative[F]

  def applicativeIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> (F.pure((a: A) => a) <*> fa)

  def applicativeHomomorphism[A, B](a: A, f: A => B): IsEq[F[B]] =
    F.pure(f(a)) <-> (F.pure(f) <*> F.pure(a))

  def applicativeInterchange[A, B](a: A, ff: F[A => B]): IsEq[F[B]] =
    (ff <*> F.pure(a)) <-> (F.pure((f: A => B) => f(a)) <*> ff)

  def applicativeComposition[A, B, C](fa: F[A], ff: F[A => B], fg: F[B => C]): IsEq[F[C]] = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    (fg <*> (ff <*> fa)) <-> ((fg.map(compose) <*> ff) <*> fa)
  }

  def applicativeMap[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.map(f) <-> (F.pure(f) <*> fa)
}

object ApplicativeLaws {
  def apply[F[_]](implicit instance: Applicative[F]): ApplicativeLaws[F] = new ApplicativeLaws[F] {
    val F: Applicative[F] = instance
  }
}
