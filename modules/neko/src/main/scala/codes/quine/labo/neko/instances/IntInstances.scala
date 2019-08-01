package codes.quine.labo
package neko
package instances

import data._

trait IntInstances {
  implicit object IntInstances extends Ord[Int] {
    override def eqv(x: Int, y: Int): Boolean = x == y

    def cmp(x: Int, y: Int): Ordering =
      if (x < y) Ordering.LT else if (x > y) Ordering.GT else Ordering.EQ
  }
}
