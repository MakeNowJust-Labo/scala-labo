package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._, test._

object ReaderTProps extends Scalaprops {
  val `laws (Id)` = Properties.list(
    MonadProps[Reader[Boolean, *]].all[Int, Int, Int],
    MonoidProps[Reader[Boolean, String]].all
  )

  val `laws (Eval)` = Properties.list(
    MonadProps[ReaderT[Eval, Boolean, *]].all[Int, Int, Int],
    MonoidProps[ReaderT[Eval, Boolean, String]].all
  )

  val `laws (Option)` = Properties.list(
    MonadProps[ReaderT[Option, Boolean, *]].all[Int, Int, Int],
    AlternativeProps[ReaderT[Option, Boolean, *]].all[Int, Int, Int],
    MonoidProps[ReaderT[Option, Boolean, String]].all
  )
}
