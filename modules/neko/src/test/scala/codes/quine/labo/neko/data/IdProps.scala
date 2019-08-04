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
    EqProps[Id[Int]].eq,
    PartialOrdProps[Id[Int]].partialOrd,
    OrdProps[Id[Int]].ord,
    HashProps[Id[Int]].hash,
    SemigroupProps[Id[String]].semigroup,
    MonoidProps[Id[String]].monoid,
    FunctorProps[Id].functor[Int, Int, Int],
    ApplicativeProps[Id].applicative[Int, Int, Int],
    MonadProps[Id].monad[Int, Int, Int]
  )
}
