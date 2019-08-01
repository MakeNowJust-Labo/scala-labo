package codes.quine.labo
package neko

import scala.math

import simulacrum.typeclass

import data._

@typeclass trait Ord[A] extends PartialOrd[A] { self =>
  @simulacrum.op("<=>", alias = true)
  def cmp(x: A, y: A): Ordering

  final def compare(x: A, y: A): Int = cmp(x, y).toInt

  override def tryCmp(x: A, y: A): Option[Ordering] = Some(cmp(x, y))

  override def eqv(x: A, y: A): Boolean = compare(x, y) == 0
  override def lteqv(x: A, y: A): Boolean = compare(x, y) <= 0
  override def lt(x: A, y: A): Boolean = compare(x, y) < 0
  override def gteqv(x: A, y: A): Boolean = compare(x, y) >= 0
  override def gt(x: A, y: A): Boolean = compare(x, y) > 0

  def min(x: A, y: A): A = {
    val n = compare(x, y)
    if (n <= 0) x
    else y
  }

  def max(x: A, y: A): A = {
    val n = compare(x, y)
    if (n >= 0) x
    else y
  }

  @simulacrum.noop
  override def by[B](f: B => A): Ord[B] = Ord.cmp((x, y) => self.cmp(f(x), f(y)))

  @simulacrum.noop
  override def reverse: Ord[A] = Ord.cmp((x, y) => self.cmp(x, y).reverse)
}

object Ord {
  def cmp[A](f: (A, A) => Ordering): Ord[A] = new Ord[A] {
    def cmp(x: A, y: A): Ordering = f(x, y)
  }

  def fromOrdering[A](implicit instance: math.Ordering[A]): Ord[A] = new Ord[A] {
    def cmp(x: A, y: A): Ordering = Ordering.fromInt(instance.compare(x, y))
  }
}
