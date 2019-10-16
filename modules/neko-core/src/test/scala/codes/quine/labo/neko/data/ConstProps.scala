package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, test._, props._

object ConstProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Const[Int, Int]].all,
    HashProps[Const[Int, Int]].all,
    MonoidProps[Const[String, Int]].all,
    ApplicativeProps[Const[String, *]].all[Int, Int, Int],
    ContravariantProps[Const[Int, *]].all[Int, Int, Int]
  )
}
