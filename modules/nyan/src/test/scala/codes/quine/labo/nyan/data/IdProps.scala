package codes.quine.labo.nyan
package data

import props._, instances._

import scalaprops._

object IdProps extends Scalaprops {
  implicit def IdGen[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))

  val laws = Properties.list(
    FunctorProps[Id].functor[MinInt, MinInt, MinInt],
    ApplicativeProps[Id].applicative[MinInt, MinInt, MinInt],
    MonadProps[Id].monad[MinInt, MinInt, MinInt],
  )
}
