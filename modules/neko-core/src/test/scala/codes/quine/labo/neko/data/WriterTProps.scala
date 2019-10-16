package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._, test._

object WriterTProps extends Scalaprops {
  val `laws (Id)` = Properties.list(
    OrdProps[Writer[String, Int]].all,
    HashProps[Writer[String, Int]].all,
    MonoidProps[Writer[String, String]].all,
    MonadProps[Writer[String, *]].all[Int, Int, Int]
  )

  val `laws (Eval)` = Properties.list(
    OrdProps[WriterT[Eval, String, Int]].all,
    HashProps[WriterT[Eval, String, Int]].all,
    MonoidProps[WriterT[Eval, String, String]].all,
    MonadProps[WriterT[Eval, String, *]].all[Int, Int, Int]
  )

  val `laws (Option)` = Properties.list(
    OrdProps[WriterT[Option, String, Int]].all,
    HashProps[WriterT[Option, String, Int]].all,
    MonoidProps[WriterT[Option, String, String]].all,
    MonadProps[WriterT[Option, String, *]].all[Int, Int, Int],
    AlternativeProps[WriterT[Option, String, *]].all[Int, Int, Int]
  )
}
