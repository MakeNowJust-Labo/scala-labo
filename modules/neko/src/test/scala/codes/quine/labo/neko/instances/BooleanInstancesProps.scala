package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object BooleanInstancesProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Boolean].all,
    HashProps[Boolean].all
  )
}
