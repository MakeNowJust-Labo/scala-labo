package codes.quine.labo.nyan
package props

import scalaprops._

sealed abstract class MinInt(val n: Int) {
  def +(other: MinInt): MinInt = MinInt(n + other.n)
}

object MinInt {
  def apply(n: Int): MinInt = n.abs % 5 match {
    case 0 => X0
    case 1 => X1
    case 2 => X2
    case 3 => X3
    case 4 => X4
  }

  object X0 extends MinInt(0)
  object X1 extends MinInt(1)
  object X2 extends MinInt(2)
  object X3 extends MinInt(3)
  object X4 extends MinInt(4)

  implicit object MinIntInstances extends Eq[MinInt] with ExhaustiveCheck[MinInt] {
    def eqv(x: MinInt, y: MinInt): Boolean = x.n == y.n

    def values: Set[MinInt] = Set(X0, X1, X2, X3, X4)
  }

  implicit def MinIntGen: Gen[MinInt] = Gen.genIntAll.map(MinInt(_))
  implicit def MinIntCogen: Cogen[MinInt] = Cogen.cogenInt.contramap(_.n)
}