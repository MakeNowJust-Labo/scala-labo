package codes.quine.labo.nyan
package props

import laws._

import scalaprops._

trait FunctorProps[F[_]] {
  val laws: FunctorLaws[F]

  implicit val F: Functor[F] = laws.F

  def functorIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.functorIdentity(_: F[A]))

  def functorComposition[A, B, C](implicit gfa: Gen[F[A]], gf: Gen[A => B], gg: Gen[B => C], efc: Eq[F[C]]): Property =
    Property.forAll(laws.functorComposition(_: F[A], _: A => B, _: B => C))

  def functor[A, B, C](implicit gfa: Gen[F[A]],
                       gf: Gen[A => B],
                       gg: Gen[B => C],
                       efa: Eq[F[A]],
                       efc: Eq[F[C]]): Properties[NyanLaw] =
    Properties.properties(NyanLaw.functor)(
      NyanLaw.functorIdentity -> functorIdentity,
      NyanLaw.functorComposition -> functorComposition,
    )
}

object FunctorProps {
  def apply[F[_]](implicit instance: Functor[F]): FunctorProps[F] = new FunctorProps[F] {
    override lazy val laws: FunctorLaws[F] = FunctorLaws[F](instance)
  }
}
