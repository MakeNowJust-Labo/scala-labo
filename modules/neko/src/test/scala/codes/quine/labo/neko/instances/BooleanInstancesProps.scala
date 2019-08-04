package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object BooleanInstancesProps extends Scalaprops {
  val laws = Properties.list(
    EqProps[Boolean].eq,
    PartialOrdProps[Boolean].partialOrd,
    OrdProps[Boolean].ord,
    HashProps[Boolean].hash
  )
}
