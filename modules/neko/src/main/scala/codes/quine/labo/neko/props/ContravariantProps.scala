package codes.quine.labo
package neko
package props

import laws._

import scalaprops._

trait ContravariantProps[F[_]] {
  val laws: ContravariantLaws[F]

  def contravariantIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.contravariantIdentity(_: F[A]))

  def contravariantComposition[A, B, C](implicit gfa: Gen[F[A]],
                                        gf: Gen[B => A],
                                        gg: Gen[C => B],
                                        efc: Eq[F[C]]): Property =
    Property.forAll(laws.contravariantComposition(_: F[A], _: B => A, _: C => B))

  def props[A, B, C](implicit gfa: Gen[F[A]],
                     gf: Gen[B => A],
                     gg: Gen[C => B],
                     efa: Eq[F[A]],
                     efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.contravariant)(
      NekoLaw.contravariantIdentity -> contravariantIdentity[A],
      NekoLaw.contravariantComposition -> contravariantComposition[A, B, C]
    )

  def all[A, B, C](implicit gfa: Gen[F[A]],
                   gf: Gen[B => A],
                   gg: Gen[C => B],
                   efa: Eq[F[A]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.contravariantAll, props[A, B, C])
}

object ContravariantProps {
  def apply[F[_]: Contravariant]: ContravariantProps[F] = new ContravariantProps[F] {
    val laws: ContravariantLaws[F] = ContravariantLaws[F]
  }
}
