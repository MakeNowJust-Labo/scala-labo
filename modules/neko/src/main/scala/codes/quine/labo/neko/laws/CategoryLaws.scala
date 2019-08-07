package codes.quine.labo
package neko
package laws

import syntax._

trait CategoryLaws[F[_, _]] {
  implicit val F: Category[F]

  def categoryLeftIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    (F.identity >>> fab) <-> fab

  def categoryRightIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    (fab >>> F.identity) <-> fab
}

object CategoryLaws {
  def apply[F[_, _]: Category]: CategoryLaws[F] = new CategoryLaws[F] {
    val F: Category[F] = Category[F]
  }
}
