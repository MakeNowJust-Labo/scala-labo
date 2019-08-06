package codes.quine.labo
package neko
package instances

import data._

trait IntInstances extends IntInstances0

private[instances] trait IntInstances0 extends IntInstances1 {
  implicit val intOrdInstance: Ord[Int] = new Ord[Int] {
    override def eqv(x: Int, y: Int): Boolean = x == y

    def cmp(x: Int, y: Int): Ordering =
      if (x < y) Ordering.LT else if (x > y) Ordering.GT else Ordering.EQ
  }
}

private[instances] trait IntInstances1 {
  implicit val intHashInstance: Hash[Int] = new Hash[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
    def hash(x: Int): Int = x.hashCode
  }
}

package object int extends IntInstances {
  def sumMonoidInstance: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def concat(x: Int, y: Int): Int = x + y
  }

  def productMonoidInstance: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 1
    def concat(x: Int, y: Int): Int = x * y
  }
}
