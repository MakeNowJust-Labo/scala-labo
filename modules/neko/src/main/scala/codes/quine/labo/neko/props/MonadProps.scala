package codes.quine.labo.neko
package props

import laws._

import scalaprops._

trait MonadProps[F[_]] extends ApplicativeProps[F] {
  val laws: MonadLaws[F]

  def monadLeftIdentity[A, B](implicit ga: Gen[A], gf: Gen[A => F[B]], efb: Eq[F[B]]): Property =
    Property.forAll(laws.monadLeftIdentity(_: A, _: A => F[B]))

  def monadRightIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monadRightIdentity(_: F[A]))

  def monadAssociativity[A, B, C](implicit gfa: Gen[F[A]],
                                  gf: Gen[A => F[B]],
                                  gg: Gen[B => F[C]],
                                  efc: Eq[F[C]]): Property =
    Property.forAll(laws.monadAssociativity(_: F[A], _: A => F[B], _: B => F[C]))

  def tailRecMStackSafety(implicit ef: Eq[F[Int]]): Property = Property.prop(laws.tailRecMStackSafety)

  def monad[A, B, C](implicit ga: Gen[A],
                     gfa: Gen[F[A]],
                     gf: Gen[A => F[B]],
                     gg: Gen[B => F[C]],
                     efa: Eq[F[A]],
                     efb: Eq[F[B]],
                     efc: Eq[F[C]],
                     ef: Eq[F[Int]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.monad)(
      NekoLaw.monadLeftIdentity -> monadLeftIdentity[A, B],
      NekoLaw.monadRightIdentity -> monadRightIdentity[A],
      NekoLaw.monadAssociativity -> monadAssociativity[A, B, C],
      NekoLaw.monadTailRecMStackSafety -> tailRecMStackSafety
    )
}

object MonadProps {
  def apply[F[_]](implicit instance: Monad[F]): MonadProps[F] = new MonadProps[F] {
    val laws: MonadLaws[F] = MonadLaws[F](instance)
  }
}
