package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait ProfunctorProps[F[_, _]] {
  val laws: ProfunctorLaws[F]

  def profunctorDimapIdentity[A, B](implicit gfab: Gen[F[A, B]], efab: Eq[F[A, B]]): Property =
    Property.forAll(laws.profunctorDimapIdentity(_: F[A, B]))

  def profunctorDimapComposition[A1, A0, A, B, B0, B1](implicit gfab: Gen[F[A, B]],
                                                       gf1: Gen[A0 => A],
                                                       gf2: Gen[A1 => A0],
                                                       gg1: Gen[B => B0],
                                                       gg2: Gen[B0 => B1],
                                                       efa1b1: Eq[F[A1, B1]]): Property =
    Property.forAll(laws.profunctorDimapComposition(_: F[A, B], _: A0 => A, _: A1 => A0, _: B => B0, _: B0 => B1))

  def props[A1, A0, A, B, B0, B1](implicit gfab: Gen[F[A, B]],
                                  gf1: Gen[A0 => A],
                                  gf2: Gen[A1 => A0],
                                  gg1: Gen[B => B0],
                                  gg2: Gen[B0 => B1],
                                  efab: Eq[F[A, B]],
                                  efa1b1: Eq[F[A1, B1]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.profunctor)(
      NekoLaw.profunctorDimapIdentity -> profunctorDimapIdentity[A, B],
      NekoLaw.profunctorDimapComposition -> profunctorDimapComposition[A1, A0, A, B, B0, B1]
    )

  def all[A1, A0, A, B, B0, B1](implicit gfab: Gen[F[A, B]],
                                gf1: Gen[A0 => A],
                                gf2: Gen[A1 => A0],
                                gg1: Gen[B => B0],
                                gg2: Gen[B0 => B1],
                                efab: Eq[F[A, B]],
                                efa1b1: Eq[F[A1, B1]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.profunctorAll, props[A1, A0, A, B, B0, B1])
}

object ProfunctorProps {
  def apply[F[_, _]: Profunctor]: ProfunctorProps[F] = new ProfunctorProps[F] {
    val laws: ProfunctorLaws[F] = ProfunctorLaws[F]
  }
}
