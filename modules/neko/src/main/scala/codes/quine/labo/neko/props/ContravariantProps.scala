package codes.quine.labo.neko
package props

import laws._

import scalaprops._

trait ContravariantProps[F[_]] {
  val laws: ContravariantLaws[F]

  implicit def F: Contravariant[F] = laws.F

  def contravariantIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.contravariantIdentity(_: F[A]))

  def contravariantComposition[A, B, C](implicit gfa: Gen[F[A]],
                                        gf: Gen[B => A],
                                        gg: Gen[C => B],
                                        efc: Eq[F[C]]): Property =
    Property.forAll(laws.contravariantComposition(_: F[A], _: B => A, _: C => B))

  def contravariant[A, B, C](implicit gfa: Gen[F[A]],
                             gf: Gen[B => A],
                             gg: Gen[C => B],
                             efa: Eq[F[A]],
                             efc: Eq[F[C]]): Properties[NekoLaw] = Properties.properties(NekoLaw.contravariant)(
    NekoLaw.contravariantIdentity -> contravariantIdentity[A],
    NekoLaw.contravariantComposition -> contravariantComposition[A, B, C]
  )
}

object ContravariantProps {
  def apply[F[_]](implicit instance: Contravariant[F]): ContravariantProps[F] = new ContravariantProps[F] {
    override lazy val laws: ContravariantLaws[F] = ContravariantLaws[F](instance)
  }
}
