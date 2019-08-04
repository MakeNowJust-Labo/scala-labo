package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait FunctorProps[F[_]] {
  val laws: FunctorLaws[F]

  def functorIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.functorIdentity(_: F[A]))

  def functorComposition[A, B, C](implicit gfa: Gen[F[A]], gf: Gen[A => B], gg: Gen[B => C], efc: Eq[F[C]]): Property =
    Property.forAll(laws.functorComposition(_: F[A], _: A => B, _: B => C))

  def props[A, B, C](implicit gfa: Gen[F[A]],
                     gf: Gen[A => B],
                     gg: Gen[B => C],
                     efa: Eq[F[A]],
                     efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.functor)(
      NekoLaw.functorIdentity -> functorIdentity,
      NekoLaw.functorComposition -> functorComposition
    )

  def all[A, B, C](implicit gfa: Gen[F[A]],
                   gf: Gen[A => B],
                   gg: Gen[B => C],
                   efa: Eq[F[A]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.functorAll, props[A, B, C])
}

object FunctorProps {
  def apply[F[_]: Functor]: FunctorProps[F] = new FunctorProps[F] {
    val laws: FunctorLaws[F] = FunctorLaws[F]
  }
}
