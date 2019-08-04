package codes.quine.labo
package neko
package laws

import syntax._

trait MonoidLaws[A] extends SemigroupLaws[A] {
  implicit val A: Monoid[A]

  def monoidLeftIdentity(x: A): IsEq[A] =
    x <-> (A.empty |+| x)

  def monoidRightIdentity(x: A): IsEq[A] =
    x <-> (x |+| A.empty)
}

object MonoidLaws {
  def apply[A](implicit instance: Monoid[A]): MonoidLaws[A] = new MonoidLaws[A] {
    val A: Monoid[A] = instance
  }
}
