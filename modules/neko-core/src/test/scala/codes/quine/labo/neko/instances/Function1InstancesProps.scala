package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object Function1InstancesProps extends Scalaprops {
  val laws = Properties.list(
    ArrowProps[Function1].all[Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean]
  )
}
