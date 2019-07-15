package codes.quine.labo.nyan
package laws

import syntax._

trait AlternativeLaws[F[_]] extends ApplicativeLaws[F] with MonoidKLaws[F] {
  implicit override val F: Alternative[F]

  def alternativeRightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    F.empty[B] <-> (ff <*> F.empty[A])

  def alternativeLeftDistributivity[A, B](x: F[A], y: F[A], f: A => B): IsEq[F[B]] =
    (x <+> y).map(f) <-> (x.map(f) <+> y.map(f))

  def alternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    ((ff <+> fg) <*> fa) <-> ((ff <*> fa) <+> (fg <*> fa))
}

object AlternativeLaws {
  def apply[F[_]](implicit instance: Alternative[F]): AlternativeLaws[F] = new AlternativeLaws[F] {
    implicit override val F: Alternative[F] = instance
  }
}