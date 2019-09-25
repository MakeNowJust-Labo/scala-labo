package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait StrongProps[F[_, _]] {
  val laws: StrongLaws[F]
  import laws._

  def strongFirstSecondDimap[A, B, C](implicit gfab: Gen[F[A, B]], efacbc: Eq[F[(A, C), (B, C)]]): Property =
    Property.forAll(laws.strongFirstSecondDimap[A, B, C](_: F[A, B]))

  def strongSecondFirstDimap[A, B, C](implicit gfab: Gen[F[A, B]], efcacb: Eq[F[(C, A), (C, B)]]): Property =
    Property.forAll(laws.strongSecondFirstDimap[A, B, C](_: F[A, B]))

  def strongLmapFirstRmap[A, B, C](implicit gfab: Gen[F[A, B]], efacb: Eq[F[(A, C), B]]): Property =
    Property.forAll(laws.strongLmapFirstRmap[A, B, C](_: F[A, B]))

  def strongLmapSecondRmap[A, B, C](implicit gfab: Gen[F[A, B]], efcab: Eq[F[(C, A), B]]): Property =
    Property.forAll(laws.strongLmapSecondRmap[A, B, C](_: F[A, B]))

  def strongFirstDinaturality[A, B, C, D](implicit gfab: Gen[F[A, B]],
                                          gf: Gen[C => D],
                                          efacbd: Eq[F[(A, C), (B, D)]]): Property =
    Property.forAll(laws.strongFirstDinaturality(_: F[A, B], _: C => D))

  def strongSecondDinaturality[A, B, C, D](implicit gfab: Gen[F[A, B]],
                                           gf: Gen[C => D],
                                           efcadb: Eq[F[(C, A), (D, B)]]): Property =
    Property.forAll(laws.strongSecondDinaturality(_: F[A, B], _: C => D))

  def strongFirstFirstDimap[A, B, C, D](implicit gfab: Gen[F[A, B]],
                                        efacdbcd: Eq[F[((A, C), D), ((B, C), D)]]): Property =
    Property.forAll(laws.strongFirstFirstDimap[A, B, C, D](_: F[A, B]))

  def strongSecondSecondDimap[A, B, C, D](implicit gfab: Gen[F[A, B]],
                                          efdcadcb: Eq[F[(D, (C, A)), (D, (C, B))]]): Property =
    Property.forAll(laws.strongSecondSecondDimap[A, B, C, D](_: F[A, B]))

  def props[A, B, C, D](implicit gfab: Gen[F[A, B]],
                        gf: Gen[C => D],
                        efacbc: Eq[F[(A, C), (B, C)]],
                        efcacb: Eq[F[(C, A), (C, B)]],
                        efacb: Eq[F[(A, C), B]],
                        efcab: Eq[F[(C, A), B]],
                        efacbd: Eq[F[(A, C), (B, D)]],
                        efcadb: Eq[F[(C, A), (D, B)]],
                        efacdbcd: Eq[F[((A, C), D), ((B, C), D)]],
                        efdcadcb: Eq[F[(D, (C, A)), (D, (C, B))]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.strong)(
      NekoLaw.strongFirstSecondDimap -> strongFirstSecondDimap[A, B, C],
      NekoLaw.strongSecondFirstDimap -> strongSecondFirstDimap[A, B, C],
      NekoLaw.strongLmapFirstRmap -> strongLmapFirstRmap[A, B, C],
      NekoLaw.strongLmapSecondRmap -> strongLmapSecondRmap[A, B, C],
      NekoLaw.strongFirstDinaturality -> strongFirstDinaturality[A, B, C, D],
      NekoLaw.strongSecondDinaturality -> strongSecondDinaturality[A, B, C, D],
      NekoLaw.strongFirstFirstDimap -> strongFirstFirstDimap[A, B, C, D],
      NekoLaw.strongSecondSecondDimap -> strongSecondSecondDimap[A, B, C, D]
    )

  def all[A1, A0, A, B, B0, B1, C, D](implicit gfab: Gen[F[A, B]],
                                      gf: Gen[C => D],
                                      gf1: Gen[A0 => A],
                                      gf2: Gen[A1 => A0],
                                      gg1: Gen[B => B0],
                                      gg2: Gen[B0 => B1],
                                      efab: Eq[F[A, B]],
                                      efa1b1: Eq[F[A1, B1]],
                                      efacbc: Eq[F[(A, C), (B, C)]],
                                      efcacb: Eq[F[(C, A), (C, B)]],
                                      efacb: Eq[F[(A, C), B]],
                                      efcab: Eq[F[(C, A), B]],
                                      efacbd: Eq[F[(A, C), (B, D)]],
                                      efcadb: Eq[F[(C, A), (D, B)]],
                                      efacdbcd: Eq[F[((A, C), D), ((B, C), D)]],
                                      efdcadcb: Eq[F[(D, (C, A)), (D, (C, B))]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.strongAll, ProfunctorProps[F].all[A1, A0, A, B, B0, B1], props[A, B, C, D])
}

object StrongProps {
  def apply[F[_, _]: Strong]: StrongProps[F] = new StrongProps[F] {
    val laws: StrongLaws[F] = StrongLaws[F]
  }
}
