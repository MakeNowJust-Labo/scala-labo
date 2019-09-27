package codes.quine.labo
package neko
package data

import scalaprops._
import props._, instances._

object NestedProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))
  implicit def idCogenInstance[A](implicit ca: Cogen[A]): Cogen[Id[A]] = Cogen[A].contramap(_.value)
  implicit def nestedGenInstance[F[_], G[_], A](implicit gfga: Gen[F[G[A]]]): Gen[Nested[F, G, A]] =
    Gen[F[G[A]]].map(Nested(_))
  implicit def nestedCogenInstance[F[_], G[_], A](implicit cfga: Cogen[F[G[A]]]): Cogen[Nested[F, G, A]] =
    Cogen[F[G[A]]].contramap(_.value)

  val laws = Properties.list(
    OrdProps[Nested[Id, Id, Int]].all,
    HashProps[Nested[Id, Id, Int]].all,
    ApplicativeProps[Nested[Id, Id, *]].all[Int, Int, Int],
    FoldProps[Nested[Id, Id, *]].all[Int, String]
  )
}
