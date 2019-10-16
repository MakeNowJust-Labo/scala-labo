package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, test._, props._

object ContTProps extends Scalaprops {
  val laws = Properties.list(
    MonadProps[Cont[Int, *]].all[Int, Int, Int],
    DeferProps[Cont[Int, *]].all[Int]
  )
}
