package codes.quine.labo.nyan

import laws._, syntax._

package object props {
  implicit def isEqToBoolean[A](isEq: IsEq[A])(implicit ea: Eq[A]): Boolean = isEq.lhs === isEq.rhs
}
