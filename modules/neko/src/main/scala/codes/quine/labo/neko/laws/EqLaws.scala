package codes.quine.labo.neko
package laws

import syntax._

trait EqLaws[A] {
  implicit val EA: Eq[A]

  def eqReflexivity(x: A): IsEq[A] =
    x <-> x

  def eqSymmetry(x: A, y: A): IsEq[Boolean] =
    (x === y) <-> (y === x)

  def eqAntiSymmetry(x: A, y: A, f: A => A): IsEq[Boolean] =
    (x =!= y || f(x) === f(y)) <-> true

  def eqTransivity(x: A, y: A, z: A): IsEq[Boolean] =
    (!(x === y && y === z) || x === z) <-> true
}

object EqLaws {
  def apply[A](implicit instance: Eq[A]): EqLaws[A] = new EqLaws[A] {
    implicit val EA: Eq[A] = instance
  }
}
