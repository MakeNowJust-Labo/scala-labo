package codes.quine.labo.nyan
package laws

import syntax._, instances._

trait FunctorLaws[F[_]] {
  implicit val F: Functor[F]

  def functorIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> fa.map((a: A) => a)

  def functorComposition[A, B, C](fa: F[A], f: A => B, g: B => C): IsEq[F[C]] =
    fa.map(f).map(g) <-> fa.map(f >>> g)
}

object FunctorLaws {
  def apply[F[_]](implicit instance: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
    override implicit val F: Functor[F] = instance
  }
}
