package codes.quine.labo
package neko
package data

import instances._, syntax._

sealed abstract class Ordering(val toInt: Int) {
  import Ordering._

  def reverse: Ordering = this match {
    case LT => GT
    case GT => LT
    case EQ => EQ
  }
}

object Ordering {
  final case object LT extends Ordering(-1)
  final case object EQ extends Ordering(0)
  final case object GT extends Ordering(1)

  def fromInt(n: Int): Ordering =
    if (n < 0) LT
    else if (n > 0) GT
    else EQ

  implicit object OrderingInstances extends Monoid[Ordering] with Ord[Ordering] {
    def empty: Ordering = EQ
    def concat(x: Ordering, y: Ordering): Ordering =
      x match {
        case EQ => y
        case _  => x
      }

    def cmp(x: Ordering, y: Ordering): Ordering = x.toInt <=> y.toInt
  }
}
