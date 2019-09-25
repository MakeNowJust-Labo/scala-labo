package codes.quine.labo
package neko
package laws

import instances.function1._, syntax._

trait ProfunctorLaws[F[_, _]] {
  implicit val F: Profunctor[F]

  def profunctorDimapIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab.dimap(identity[A])(identity[B]) <-> fab

  def profunctorDimapComposition[A1, A0, A, B, B0, B1](fab: F[A, B],
                                                       f1: A0 => A,
                                                       f2: A1 => A0,
                                                       g1: B => B0,
                                                       g2: B0 => B1): IsEq[F[A1, B1]] =
    fab.dimap(f1)(g1).dimap(f2)(g2) <-> fab.dimap(f2 >>> f1)(g1 >>> g2)
}

object ProfunctorLaws {
  def apply[F[_, _]: Profunctor]: ProfunctorLaws[F] = new ProfunctorLaws[F] {
    val F: Profunctor[F] = Profunctor[F]
  }
}
