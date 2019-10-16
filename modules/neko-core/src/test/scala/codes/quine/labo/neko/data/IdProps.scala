package codes.quine.labo
package neko
package data

import scalaprops._
import props._, test._, instances._

object IdProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Id[Int]].all,
    HashProps[Id[Int]].all,
    MonoidProps[Id[String]].all,
    BimonadProps[Id].all[Int, Int, Int],
    TraverseProps[Id].all[Id, Id, Int, Int, Int, String]
  )
}
