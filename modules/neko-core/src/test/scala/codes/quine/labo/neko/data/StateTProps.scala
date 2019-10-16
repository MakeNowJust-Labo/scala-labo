package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._, test._

object StateTProps extends Scalaprops {
  val `laws (Id)` = Properties.list(
    MonadProps[State[Boolean, *]].all[Int, Int, Int]
  )

  val `laws (Eval)` = Properties.list(
    MonadProps[StateT[Eval, Boolean, *]].all[Int, Int, Int]
  )

  val `laws (Option)` = Properties.list(
    MonadProps[StateT[Option, Boolean, *]].all[Int, Int, Int],
    MonoidKProps[StateT[Option, Boolean, *]].all[Int],
    MonoidProps[StateT[List, Boolean, Boolean]].all
  )
}
