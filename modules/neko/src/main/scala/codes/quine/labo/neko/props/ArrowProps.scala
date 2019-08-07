package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait ArrowProps[F[_, _]] {
  val laws: ArrowLaws[F]
  import laws._

  def arrowIdentity[A](implicit efaa: Eq[F[A, A]]): Property =
    Property.forAll(laws.arrowIdentity[A])

  def arrowComposition[A, B, C](implicit gf: Gen[A => B], gg: Gen[B => C], efac: Eq[F[A, C]]): Property =
    Property.forAll(laws.arrowComposition(_: A => B, _: B => C))

  def arrowExtension[A, B, C](implicit gf: Gen[A => B], efacbc: Eq[F[(A, C), (B, C)]]): Property =
    Property.forAll(laws.arrowExtension[A, B, C](_: A => B))

  def arrowFunctor[A, B, C, D](implicit gfab: Gen[F[A, B]],
                               gfbc: Gen[F[B, C]],
                               efadcd: Eq[F[(A, D), (C, D)]]): Property =
    Property.forAll(laws.arrowFunctor[A, B, C, D](_: F[A, B], _: F[B, C]))

  def arrowExchange[A, B, C, D](implicit gfab: Gen[F[A, B]], gf: Gen[C => D], efacbd: Eq[F[(A, C), (B, D)]]): Property =
    Property.forAll(laws.arrowExchange(_: F[A, B], _: C => D))

  def arrowUnit[A, B, C](implicit fab: Gen[F[A, B]], efacb: Eq[F[(A, C), B]]): Property =
    Property.forAll(laws.arrowUnit[A, B, C](_: F[A, B]))

  def arrowAssociativity[A, B, C, D](implicit gfab: Gen[F[A, B]], efacdbcd: Eq[F[((A, C), D), (B, (C, D))]]): Property =
    Property.forAll(laws.arrowAssociativity[A, B, C, D](_: F[A, B]))

  def arrowSplitConsistency[A, B, C, D](implicit gfab: Gen[F[A, B]],
                                        gfcd: Gen[F[C, D]],
                                        efacbd: Eq[F[(A, C), (B, D)]]): Property =
    Property.forAll(laws.arrowSplitConsistency(_: F[A, B], _: F[C, D]))

  def arrowMergeConsistency[A, B, C](implicit gfab: Gen[F[A, B]],
                                     gfac: Gen[F[A, C]],
                                     efabc: Eq[F[A, (B, C)]]): Property =
    Property.forAll(laws.arrowMergeConsistency(_: F[A, B], _: F[A, C]))

  def props[A, B, C, D](implicit gf: Gen[A => B],
                        gg: Gen[B => C],
                        gh: Gen[C => D],
                        gfab: Gen[F[A, B]],
                        gfac: Gen[F[A, C]],
                        gfcd: Gen[F[C, D]],
                        efaa: Eq[F[A, A]],
                        efac: Eq[F[A, C]],
                        efacbc: Eq[F[(A, C), (B, C)]],
                        efacbd: Eq[F[(A, C), (B, D)]],
                        efacb: Eq[F[(A, C), B]],
                        efacdbcd: Eq[F[((A, C), D), (B, (C, D))]],
                        efabc: Eq[F[A, (B, C)]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.arrow)(
      NekoLaw.arrowIdentity -> arrowIdentity[A],
      NekoLaw.arrowComposition -> arrowComposition[A, B, C],
      NekoLaw.arrowExtension -> arrowExtension[A, B, C],
      NekoLaw.arrowExchange -> arrowExchange[A, B, C, D],
      NekoLaw.arrowUnit -> arrowUnit[A, B, C],
      NekoLaw.arrowAssociativity -> arrowAssociativity[A, B, C, D],
      NekoLaw.arrowSplitConsistency -> arrowSplitConsistency[A, B, C, D],
      NekoLaw.arrowMergeConsistency -> arrowMergeConsistency[A, B, C]
    )

  def all[A1, A0, A, B, B0, B1, C, D](implicit gfab: Gen[F[A, B]],
                                      gfac: Gen[F[A, C]],
                                      gfbc: Gen[F[B, C]],
                                      gfcd: Gen[F[C, D]],
                                      gf: Gen[A => B],
                                      gg: Gen[B => C],
                                      gh: Gen[C => D],
                                      gf1: Gen[A0 => A],
                                      gf2: Gen[A1 => A0],
                                      gg1: Gen[B => B0],
                                      gg2: Gen[B0 => B1],
                                      efaa: Eq[F[A, A]],
                                      efab: Eq[F[A, B]],
                                      efac: Eq[F[A, C]],
                                      efad: Eq[F[A, D]],
                                      efa1b1: Eq[F[A1, B1]],
                                      efacbc: Eq[F[(A, C), (B, C)]],
                                      efcacb: Eq[F[(C, A), (C, B)]],
                                      efabc: Eq[F[A, (B, C)]],
                                      efacb: Eq[F[(A, C), B]],
                                      efcab: Eq[F[(C, A), B]],
                                      efacbd: Eq[F[(A, C), (B, D)]],
                                      efcadb: Eq[F[(C, A), (D, B)]],
                                      efacdbcd: Eq[F[((A, C), D), ((B, C), D)]],
                                      efacdbcd1: Eq[F[((A, C), D), (B, (C, D))]],
                                      efdcadcb: Eq[F[(D, (C, A)), (D, (C, B))]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.arrowAll,
                         CategoryProps[F].all[A, B, C, D],
                         StrongProps[F].all[A1, A0, A, B, B0, B1, C, D],
                         props[A, B, C, D])
}

object ArrowProps {
  def apply[F[_, _]: Arrow]: ArrowProps[F] = new ArrowProps[F] {
    val laws: ArrowLaws[F] = ArrowLaws[F]
  }
}
