package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object VectorInstancesProps extends Scalaprops {
  val laws = Properties.list(
    EqProps[Vector[Int]].eq,
    PartialOrdProps[Vector[Int]].partialOrd,
    OrdProps[Vector[Int]].ord,
    HashProps[Vector[Int]].hash,
    FunctorProps[Vector].functor[Int, Int, Int],
    ApplicativeProps[Vector].applicative[Int, Int, Int],
    MonadProps[Vector].monad[Int, Int, Int],
    SemigroupKProps[Vector].semigroupK[Int],
    MonoidKProps[Vector].monoidK[Int],
    AlternativeProps[Vector].alternative[Int, Int]
  )
}
