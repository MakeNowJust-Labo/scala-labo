package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object MapInstancesProps extends Scalaprops {
  val laws = Properties.list(
    HashProps[Map[Int, Int]].all,
    FlatMapProps[Map[Int, *]].all[Int, Int, Int]
  )
}
