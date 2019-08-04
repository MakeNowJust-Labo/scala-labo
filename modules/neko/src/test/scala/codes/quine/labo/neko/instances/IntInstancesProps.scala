package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object IntInstancesProps extends Scalaprops {
  val laws = Properties.list(
    EqProps[Int].eq,
    PartialOrdProps[Int].partialOrd,
    OrdProps[Int].ord,
    HashProps[Int].hash
  )
}
