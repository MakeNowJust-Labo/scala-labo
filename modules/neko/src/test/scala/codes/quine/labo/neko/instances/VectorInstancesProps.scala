package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object VectorInstancesProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Vector[Int]].all,
    HashProps[Vector[Int]].all,
    MonoidProps[Vector[Int]].all,
    MonadProps[Vector].all[Int, Int, Int],
    CoflatMapProps[Vector].all[Int, Int, Int],
    AlternativeProps[Vector].all[Int, Int, Int]
  )
}
