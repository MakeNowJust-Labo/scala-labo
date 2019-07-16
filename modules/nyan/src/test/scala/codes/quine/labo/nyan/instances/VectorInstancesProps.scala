package codes.quine.labo.nyan
package instances

import props._

import scalaprops._

object VectorInstancesProps extends Scalaprops {
  val laws = Properties.list(
    EqProps[Vector[Int]].eq,
    FunctorProps[Vector].functor[Int, Int, Int],
    ApplicativeProps[Vector].applicative[Int, Int, Int],
    MonadProps[Vector].monad[Int, Int, Int],
    MonoidKProps[Vector].monoidK[Int],
    AlternativeProps[Vector].alternative[Int, Int]
  )
}
