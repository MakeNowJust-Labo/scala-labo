package codes.quine.labo
package neko
package data

import scalaprops._
import props._, instances._, test._

object NestedProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Nested[Id, Id, Int]].all,
    HashProps[Nested[Id, Id, Int]].all,
    ApplicativeProps[Nested[Id, Id, *]].all[Int, Int, Int],
    FoldProps[Nested[Id, Id, *]].all[Int, String]
  )
}
