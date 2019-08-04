package codes.quine.labo
package neko
package instances

import data._

trait BooleanInstances extends BooleanInstances0

private[instances] trait BooleanInstances0 extends BooleanInstances1 {
  implicit val booleanOrdInstance: Ord[Boolean] = new Ord[Boolean] {
    def cmp(x: Boolean, y: Boolean): Ordering =
      if (x == y) Ordering.EQ else if (x) Ordering.GT else Ordering.LT
  }
}

private[instances] trait BooleanInstances1 {
  implicit val booleanHashInstance: Hash[Boolean] = new Hash[Boolean] {
    def eqv(x: Boolean, y: Boolean): Boolean = x == y
    def hash(x: Boolean): Int = x.hashCode
  }
}

package object boolean extends BooleanInstances
