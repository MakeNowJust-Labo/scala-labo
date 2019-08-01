package codes.quine.labo
package neko
package instances

import data._

trait BooleanInstances {
  implicit object BooleanInstances extends Ord[Boolean] {
    def cmp(x: Boolean, y: Boolean): Ordering =
      if (x == y) Ordering.EQ else if (x) Ordering.GT else Ordering.LT
  }
}
