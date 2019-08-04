package codes.quine.labo
package neko
package data

import scalaprops._
import props._, instances._

object IdProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))
  implicit def idCogenInstance[A](implicit ca: Cogen[A]): Cogen[Id[A]] = Cogen[A].contramap(_.value)

  val laws = Properties.list(
    OrdProps[Id[Int]].all,
    HashProps[Id[Int]].all,
    MonoidProps[Id[String]].all,
    MonadProps[Id].all[Int, Int, Int]
  )
}
