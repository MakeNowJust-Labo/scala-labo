package codes.quine.labo
package neko

import scala.math

import simulacrum.typeclass

import data._

@typeclass trait PartialOrd[A] extends Eq[A] { self =>
  def tryCmp(x: A, y: A): Option[Ordering]

  final def tryCompare(x: A, y: A): Option[Int] = tryCmp(x, y).map(_.toInt)

  final def unsafeCompare(x: A, y: A): Double = tryCompare(x, y) match {
    case Some(n) => n.toDouble
    case None    => Double.NaN
  }

  override def eqv(x: A, y: A): Boolean = unsafeCompare(x, y) == 0
  def lteqv(x: A, y: A): Boolean = unsafeCompare(x, y) <= 0
  def lt(x: A, y: A): Boolean = unsafeCompare(x, y) < 0
  def gteqv(x: A, y: A): Boolean = unsafeCompare(x, y) >= 0
  def gt(x: A, y: A): Boolean = unsafeCompare(x, y) > 0

  def pmin(x: A, y: A): Option[A] = {
    val d = unsafeCompare(x, y)
    if (d <= 0) Some(x) else if (d > 0) Some(y) else None
  }

  def pmax(x: A, y: A): Option[A] = {
    val d = unsafeCompare(x, y)
    if (d >= 0) Some(x) else if (d < 0) Some(y) else None
  }

  @simulacrum.noop
  override def by[B](f: B => A): PartialOrd[B] = PartialOrd.tryCmp((x, y) => self.tryCmp(f(x), f(y)))

  @simulacrum.noop
  def reverse: PartialOrd[A] = PartialOrd.tryCmp((x, y) => self.tryCmp(x, y).map(_.reverse))
}

object PartialOrd {
  def tryCmp[A](f: (A, A) => Option[Ordering]): PartialOrd[A] = new PartialOrd[A] {
    def tryCmp(x: A, y: A): Option[Ordering] = f(x, y)
  }

  def fromPartialOrdering[A](implicit instance: math.PartialOrdering[A]): PartialOrd[A] = new PartialOrd[A] {
    override def eqv(x: A, y: A): Boolean = instance.equiv(x, y)
    def tryCmp(x: A, y: A): Option[Ordering] = instance.tryCompare(x, y).map(Ordering.fromInt(_))
  }
}
