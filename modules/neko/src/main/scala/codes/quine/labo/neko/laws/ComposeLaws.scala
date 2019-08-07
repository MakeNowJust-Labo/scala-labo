package codes.quine.labo
package neko
package laws

import syntax._

trait ComposeLaws[F[_, _]] {
  implicit val F: Compose[F]

  def composeAssociativity[A, B, C, D](fab: F[A, B], fbc: F[B, C], fcd: F[C, D]): IsEq[F[A, D]] =
    (fab >>> (fbc >>> fcd)) <-> ((fab >>> fbc) >>> fcd)
}

object ComposeLaws {
  def apply[F[_, _]: Compose]: ComposeLaws[F] = new ComposeLaws[F] {
    val F: Compose[F] = Compose[F]
  }
}
