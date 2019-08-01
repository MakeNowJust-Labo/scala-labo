package codes.quine.labo
package neko
package laws

trait PartialOrdLaws[A] extends EqLaws[A] {
  implicit val A: PartialOrd[A]

  def partialOrdReflexivity(x: A): IsEq[Boolean] =
    A.lteqv(x, x) <-> true

  def partialOrdAntisymmetry(x: A, y: A): IsEq[Boolean] =
    (A.lteqv(x, y) && A.lteqv(y, x)) ==> A.eqv(x, y)

  def partialOrdTransivity(x: A, y: A, z: A): IsEq[Boolean] =
    (A.lteqv(x, y) && A.lteqv(y, z)) ==> A.lteqv(x, z)
}

object PartialOrdLaws {
  def apply[A](implicit instance: PartialOrd[A]): PartialOrdLaws[A] = new PartialOrdLaws[A] {
    val A: PartialOrd[A] = instance
  }
}
