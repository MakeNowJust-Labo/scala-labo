package codes.quine.labo.nyan
package data

import props._, instances._

import scalaprops._

object ReaderTProps extends Scalaprops {
  val `laws (Reader)` = {
    implicit def ReaderGen[E, A](implicit gea: Gen[E => A]): Gen[Reader[E, A]] = Gen[E => A].map(Reader(_))
    implicit def ReaderEq[E, A](implicit eea: Eq[E => A]): Eq[Reader[E, A]] = Eq[E => A].by(_.run.andThen(_.value))

    Properties.list(
      FunctorProps[Reader[MinInt, ?]].functor[MinInt, MinInt, MinInt],
      ApplicativeProps[Reader[MinInt, ?]].applicative[MinInt, MinInt, MinInt],
      MonadProps[Reader[MinInt, ?]].monad[MinInt, MinInt, MinInt]
    )
  }

  val `laws (ReaderT)` = {
    implicit def ReaderTGen[E, A](implicit gea: Gen[E => List[A]]): Gen[ReaderT[E, List, A]] =
      Gen[E => List[A]].map(ReaderT(_))
    implicit def ReaderTEq[E, A](implicit eea: Eq[E => List[A]]): Eq[ReaderT[E, List, A]] =
      Eq[E => List[A]].by(_.run)

    Properties.list(
      FunctorProps[ReaderT[MinInt, List, ?]].functor[MinInt, MinInt, MinInt],
      ApplicativeProps[ReaderT[MinInt, List, ?]].applicative[MinInt, MinInt, MinInt],
      MonadProps[ReaderT[Boolean, List, ?]].monad[MinInt, MinInt, MinInt]
    )
  }
}
