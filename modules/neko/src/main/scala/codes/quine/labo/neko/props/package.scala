package codes.quine.labo.neko

import laws._, syntax._

package object props {
  implicit def isEqToBoolean[A](isEq: IsEq[A])(implicit ea: Eq[A]): Boolean = isEq.lhs === isEq.rhs

  implicit def Function1Eq[A: ExhaustiveCheck, B: Eq]: Eq[A => B] = new Eq[A => B] {
    def eqv(f: A => B, g: A => B): Boolean = ExhaustiveCheck[A].values.forall { x =>
      f(x) === g(x)
    }
  }
}
