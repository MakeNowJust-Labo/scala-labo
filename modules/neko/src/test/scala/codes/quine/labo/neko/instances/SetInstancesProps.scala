package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object SetInstancesProps extends Scalaprops {
  val laws = Properties.list(
    PartialOrdProps[Set[Int]].all,
    HashProps[Set[Int]].all,
    MonoidProps[Set[Int]].all,
    MonoidKProps[Set].all[Int]
  )
}
