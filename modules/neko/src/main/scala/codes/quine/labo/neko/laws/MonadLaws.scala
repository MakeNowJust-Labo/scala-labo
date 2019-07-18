package codes.quine.labo.neko
package laws

import syntax._

trait MonadLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override val F: Monad[F]

  def monadLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    F.pure(a).flatMap(f) <-> f(a)

  def monadRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap(F.pure) <-> fa

  def monadAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) <-> fa.flatMap(a => f(a).flatMap(g))

  lazy val tailRecMStackSafety: IsEq[F[Int]] = {
    val n = 50000
    val res = F.tailRecM(0)(i => F.pure(if (i < n) Left(i + 1) else Right(i)))
    res <-> F.pure(n)
  }
}

object MonadLaws {
  def apply[F[_]](implicit instance: Monad[F]): MonadLaws[F] = new MonadLaws[F] {
    val F: Monad[F] = instance
  }
}
