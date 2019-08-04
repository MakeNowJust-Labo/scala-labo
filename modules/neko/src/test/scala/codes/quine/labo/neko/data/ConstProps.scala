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
    EqProps[Const[Int, Int]].eq,
    PartialOrdProps[Const[Int, Int]].partialOrd,
    OrdProps[Const[Int, Int]].ord,
    HashProps[Const[Int, Int]].hash,
    SemigroupProps[Const[String, Int]].semigroup,
    MonoidProps[Const[String, Int]].monoid,
    FunctorProps[Const[Int, *]].functor[Int, Int, Int],
    ApplicativeProps[Const[String, *]].applicative[Int, Int, Int],
    ContravariantProps[Const[Int, *]].contravariant[Int, Int, Int]
  )
}
