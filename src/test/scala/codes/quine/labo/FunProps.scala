package codes.quine.labo

import scalaprops._
import fun._

object FunProps extends Scalaprops {
  val prop = Property.forAllS[Fun[String, Int]] {
    case Fun(_, _, _, f) => f("monkey") == f("banana") && f("banana") == f("elephant")
  }

  val fold = Property.forAllS[(List[Int], Int), Fun[(Int, Int), Int]] { case ((xs, z), Fun(_, _, _, f)) =>
    xs.foldRight(z)(Function.untupled(f)) == xs.foldLeft(z)(Function.untupled(f))
  }
}
