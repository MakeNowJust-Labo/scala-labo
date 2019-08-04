package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object IntInstancesProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Int].all,
    HashProps[Int].all
  )
}
