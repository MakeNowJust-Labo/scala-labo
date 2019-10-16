package codes.quine.labo
package neko
package data

import scalaprops._
import props._, instances._, test._

object EvalProps extends Scalaprops {
  val laws =
    Properties.list(
      OrdProps[Eval[Int]].all,
      HashProps[Eval[Int]].all,
      MonoidProps[Eval[String]].all,
      BimonadProps[Eval].all[Int, Int, Int],
      DeferProps[Eval].all[Int],
      FoldProps[Eval].all[Int, String]
    )
}
