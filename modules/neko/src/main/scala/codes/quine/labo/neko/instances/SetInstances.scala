package codes.quine.labo
package neko
package instances

import data._

trait SetInstances extends SetInstances0

// NOTE: these instances do not need `Eq[A]` because `Set` uses standard `==` method for checking euqality.
//       `Hash` is not needed as well.

private[instances] trait SetInstances0 extends SetInstances1 {
  implicit def setParialOrdInstance[A]: PartialOrd[Set[A]] = new PartialOrd[Set[A]] {
    def tryCmp(x: Set[A], y: Set[A]): Option[Ordering] =
      (x, y) match {
        case (xs, ys) if xs.size == ys.size => if (xs == ys) Some(Ordering.EQ) else None
        case (xs, ys) if xs.size < ys.size  => if (xs.subsetOf(ys)) Some(Ordering.LT) else None
        case (xs, ys) if xs.size > ys.size  => if (ys.subsetOf(xs)) Some(Ordering.GT) else None
        case _                              => None // here is unreachable.
      }

    override def eqv(x: Set[A], y: Set[A]): Boolean = x == y
  }

  implicit def setMonoidInstance[A]: Monoid[Set[A]] = setMonoidKInstance.algebra[A]

  implicit val setMonoidKInstance: MonoidK[Set] = new MonoidK[Set] {
    def emptyK[A]: Set[A] = Set.empty
    def concatK[A](x: Set[A], y: Set[A]): Set[A] = x | y
  }
}

private[instances] trait SetInstances1 {
  implicit def setHashInstance[A]: Hash[Set[A]] = new Hash[Set[A]] {
    def eqv(x: Set[A], y: Set[A]): Boolean = x == y
    def hash(x: Set[A]): Int = x.hashCode
  }
}

package object set extends SetInstances
