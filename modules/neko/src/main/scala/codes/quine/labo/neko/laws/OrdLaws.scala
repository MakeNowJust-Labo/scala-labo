package codes.quine.labo
package neko
package laws

trait OrdLaws[A] extends PartialOrdLaws[A] {
  implicit val A: Ord[A]

  def ordTotality(x: A, y: A): IsEq[Boolean] =
    (A.lteqv(x, y) || A.lteqv(y, x)) <-> true
}

object OrdLaws {
  def apply[A](implicit instance: Ord[A]): OrdLaws[A] = new OrdLaws[A] {
    val A: Ord[A] = instance
  }
}
