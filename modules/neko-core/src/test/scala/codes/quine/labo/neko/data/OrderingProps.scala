package codes.quine.labo
package neko
package data

import scalaprops._
import props._, test._

object OrderingProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Ordering].all,
    HashProps[Ordering].all,
    MonoidProps[Ordering].all
  )
}
