package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object OptionInstancesProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Option[Int]].all,
    HashProps[Option[Int]].all,
    MonadProps[Option].all[Int, Int, Int],
    AlternativeProps[Option].all[Int, Int, Int]
  )
}
