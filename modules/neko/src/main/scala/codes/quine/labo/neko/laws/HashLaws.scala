package codes.quine.labo
package neko
package laws

trait HashLaws[A] {
  implicit val A: Hash[A]

  def hashCompatibility(x: A, y: A): IsEq[Boolean] =
    A.eqv(x, y) ==> (A.hash(x) == A.hash(y))
}

object HashLaws {
  def apply[A: Hash]: HashLaws[A] = new HashLaws[A] {
    val A: Hash[A] = Hash[A]
  }
}
