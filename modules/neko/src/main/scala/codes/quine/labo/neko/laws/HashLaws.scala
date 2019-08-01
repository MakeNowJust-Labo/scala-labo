package codes.quine.labo
package neko
package laws

trait HashLaws[A] extends EqLaws[A] {
  implicit val A: Hash[A]

  def hashCompatibility(x: A, y: A): IsEq[Boolean] =
    A.eqv(x, y) ==> (A.hash(x) == A.hash(y))
}

object HashLaws {
  def apply[A](implicit instance: Hash[A]): HashLaws[A] = new HashLaws[A] {
    val A: Hash[A] = instance
  }
}
