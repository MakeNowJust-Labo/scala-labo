package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object ListInstancesProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[List[Int]].all,
    HashProps[List[Int]].all,
    MonadProps[List].all[Int, Int, Int],
    AlternativeProps[List].all[Int, Int, Int]
  )
}
