package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object OptionInstancesProps extends Scalaprops {
  val laws = Properties.list(
    EqProps[Option[Int]].eq,
    PartialOrdProps[Option[Int]].partialOrd,
    OrdProps[Option[Int]].ord,
    HashProps[Option[Int]].hash,
    FunctorProps[Option].functor[Int, Int, Int],
    ApplicativeProps[Option].applicative[Int, Int, Int],
    MonadProps[Option].monad[Int, Int, Int],
    SemigroupKProps[Option].semigroupK[Int],
    MonoidKProps[Option].monoidK[Int],
    AlternativeProps[Option].alternative[Int, Int]
  )
}
