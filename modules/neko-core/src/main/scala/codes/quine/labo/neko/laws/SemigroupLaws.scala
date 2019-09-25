package codes.quine.labo
package neko
package laws

import syntax._

trait SemigroupLaws[A] {
  implicit val A: Semigroup[A]

  def semigroupAssociativity(x: A, y: A, z: A): IsEq[A] =
    ((x |+| y) |+| z) <-> (x |+| (y |+| z))
}

object SemigroupLaws {
  def apply[A: Semigroup]: SemigroupLaws[A] = new SemigroupLaws[A] {
    val A: Semigroup[A] = Semigroup[A]
  }
}
