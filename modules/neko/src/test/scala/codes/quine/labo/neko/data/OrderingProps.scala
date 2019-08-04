package codes.quine.labo
package neko
package data

import scalaprops._
import props._

object OrderingProps extends Scalaprops {
  implicit val orderingGenInstance: Gen[Ordering] = Gen[Int].map(Ordering.fromInt(_))
  implicit val orderingCogenInstance: Cogen[Ordering] = Cogen[Int].contramap(_.toInt)

  val laws = Properties.list(
    OrdProps[Ordering].all,
    HashProps[Ordering].all,
    MonoidProps[Ordering].all
  )
}
