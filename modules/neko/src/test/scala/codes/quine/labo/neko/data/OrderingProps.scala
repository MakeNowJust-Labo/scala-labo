package codes.quine.labo
package neko
package data

import scalaprops._
import props._

object OrderingProps extends Scalaprops {
  implicit val orderingGenInstance: Gen[Ordering] = Gen[Int].map(Ordering.fromInt(_))
  implicit val orderingCogenInstance: Cogen[Ordering] = Cogen[Int].contramap(_.toInt)

  val laws = Properties.list(
    EqProps[Ordering].eq,
    PartialOrdProps[Ordering].partialOrd,
    OrdProps[Ordering].ord,
    HashProps[Ordering].hash,
    SemigroupProps[Ordering].semigroup,
    MonoidProps[Ordering].monoid
  )
}
