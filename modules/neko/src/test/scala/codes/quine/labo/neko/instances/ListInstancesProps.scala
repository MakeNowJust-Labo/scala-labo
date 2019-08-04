package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object ListInstancesProps extends Scalaprops {
  val laws = Properties.list(
    EqProps[List[Int]].eq,
    PartialOrdProps[List[Int]].partialOrd,
    OrdProps[List[Int]].ord,
    HashProps[List[Int]].hash,
    FunctorProps[List].functor[Int, Int, Int],
    ApplicativeProps[List].applicative[Int, Int, Int],
    MonadProps[List].monad[Int, Int, Int],
    SemigroupKProps[List].semigroupK[Int],
    MonoidKProps[List].monoidK[Int],
    AlternativeProps[List].alternative[Int, Int]
  )
}
