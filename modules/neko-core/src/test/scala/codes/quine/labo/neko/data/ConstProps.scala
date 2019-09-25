package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._

object ConstProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def constGenInstance[A: Gen, B]: Gen[Const[A, B]] = Gen[A].map(Const(_))
  implicit def constCogenInstance[A: Cogen, B]: Cogen[Const[A, B]] = Cogen[A].contramap(_.value)

  val laws = Properties.list(
    OrdProps[Const[Int, Int]].all,
    HashProps[Const[Int, Int]].all,
    MonoidProps[Const[String, Int]].all,
    ApplicativeProps[Const[String, *]].all[Int, Int, Int],
    ContravariantProps[Const[Int, *]].all[Int, Int, Int]
  )
}
