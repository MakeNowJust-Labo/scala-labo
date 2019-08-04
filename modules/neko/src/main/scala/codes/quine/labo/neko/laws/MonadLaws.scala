package codes.quine.labo
package neko
package laws

import syntax._

trait MonadLaws[F[_]] {
  implicit val F: Monad[F]

  def monadLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    F.pure(a).flatMap(f) <-> f(a)

  def monadRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap(F.pure) <-> fa

  lazy val monadTailRecMStackSafety: IsEq[F[Int]] = {
    val n = 50000
    val res = F.tailRecM(0)(i => F.pure(if (i < n) Left(i + 1) else Right(i)))
    res <-> F.pure(n)
  }
}

object MonadLaws {
  def apply[F[_]: Monad]: MonadLaws[F] = new MonadLaws[F] {
    val F: Monad[F] = Monad[F]
  }
}
