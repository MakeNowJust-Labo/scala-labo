package codes.quine.labo
package neko
package laws

import syntax._

trait EqLaws[A] {
  implicit val A: Eq[A]

  def eqReflexivity(x: A): IsEq[A] =
    x <-> x

  def eqSymmetry(x: A, y: A): IsEq[Boolean] =
    (x === y) <-> (y === x)

  def eqAntiSymmetry(x: A, y: A, f: A => A): IsEq[Boolean] =
    x === y ==> (f(x) === f(y))

  def eqTransivity(x: A, y: A, z: A): IsEq[Boolean] =
    (x === y && y === z) ==> (x === z)
}

object EqLaws {
  def apply[A](implicit instance: Eq[A]): EqLaws[A] = new EqLaws[A] {
    val A: Eq[A] = instance
  }
}
