package codes.quine.labo
package neko
package laws

import syntax._

trait ApplyLaws[F[_]] {
  implicit val F: Apply[F]

  def applyComposition[A, B, C](fa: F[A], ff: F[A => B], fg: F[B => C]): IsEq[F[C]] = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    (fg <*> (ff <*> fa)) <-> ((fg.map(compose) <*> ff) <*> fa)
  }
}

object ApplyLaws {
  def apply[F[_]](implicit instance: Apply[F]): ApplyLaws[F] = new ApplyLaws[F] {
    val F: Apply[F] = instance
  }
}
