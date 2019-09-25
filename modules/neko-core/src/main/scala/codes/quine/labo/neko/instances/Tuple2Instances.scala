package codes.quine.labo
package neko
package instances

import syntax._

trait Tuple2Instances extends Tuple2Instances0

private[instances] trait Tuple2Instances0 extends Tuple2Instances1 {
  implicit def tuple2EqInstance[A: Eq, B: Eq]: Eq[(A, B)] = Eq.eqv {
    case ((a1, b1), (a2, b2)) => a1 === a2 && b1 === b2
  }

  implicit def tuple2PartialOrdInstance[A: PartialOrd, B: PartialOrd]: PartialOrd[(A, B)] = PartialOrd.tryCmp {
    case ((a1, b1), (a2, b2)) => a1.tryCmp(a2) |+| b1.tryCmp(b2)
  }

  implicit def tuple2OrdInstance[A: Ord, B: Ord]: Ord[(A, B)] = Ord.cmp {
    case ((a1, b1), (a2, b2)) => (a1 <=> a2) |+| (b1 <=> b2)
  }

  implicit def tuple2Semigroup[A: Semigroup, B: Semigroup]: Semigroup[(A, B)] = new Semigroup[(A, B)] {
    def concat(x: (A, B), y: (A, B)): (A, B) = (x._1 |+| y._1, x._2 |+| y._2)
  }

  implicit def tuple2Monoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
    def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
    def concat(x: (A, B), y: (A, B)): (A, B) = (x._1 |+| y._1, x._2 |+| y._2)
  }
}

private[instances] trait Tuple2Instances1 {
  implicit def tuple2HashInstance[A: Hash, B: Hash]: Hash[(A, B)] = new Hash[(A, B)] {
    def eqv(x: (A, B), y: (A, B)): Boolean = x._1 === y._1 && x._2 === y._2
    def hash(x: (A, B)): Int = x._1.hash * 31 + x._2.hash
  }
}

package object tuple2 extends Tuple2Instances
