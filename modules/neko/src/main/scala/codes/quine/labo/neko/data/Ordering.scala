package codes.quine.labo
package neko
package data

import instances.int._

sealed abstract class Ordering(val toInt: Int) {
  import Ordering._

  def reverse: Ordering = this match {
    case LT => GT
    case GT => LT
    case EQ => EQ
  }
}

object Ordering extends OrderingInstances0 {
  final case object LT extends Ordering(-1)
  final case object EQ extends Ordering(0)
  final case object GT extends Ordering(1)

  def fromInt(n: Int): Ordering =
    if (n < 0) LT
    else if (n > 0) GT
    else EQ
}

private[data] trait OrderingInstances0 extends OrderingInstances1 {
  import Ordering._

  implicit val orderingOrdInstance: Ord[Ordering] = Ord[Int].by(_.toInt)

  implicit val orderingMonoidInstance: Monoid[Ordering] = new Monoid[Ordering] {
    def empty: Ordering = EQ
    def concat(x: Ordering, y: Ordering): Ordering =
      x match {
        case EQ => y
        case _  => x
      }
  }
}

private[data] trait OrderingInstances1 {
  implicit val orderingHashInstance: Hash[Ordering] = new Hash[Ordering] {
    def eqv(x: Ordering, y: Ordering): Boolean = x == y
    def hash(x: Ordering): Int = x.hashCode
  }
}
