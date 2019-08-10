package codes.quine.labo

import scalaprops._

object FunProps extends Scalaprops {
  val prop = Property.forAllS[Fun[Boolean, Int]] {
    case Fun(_, _, _, f) => f(false) == f(true)
  }
}
