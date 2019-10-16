package codes.quine.labo
package neko
package props

import scalaprops._
import data._, laws._, instances._

trait TraverseProps[F[_]] {
  val laws: TraverseLaws[F]
  import laws._

  def traverseIdentity[A, B](implicit gfa: Gen[F[A]], gf: Gen[A => B], efb: Eq[F[B]]): Property =
    Property.forAll(laws.traverseIdentity(_: F[A], _: A => B))

  def traverseFoldMap[A, M: Monoid](implicit gfa: Gen[F[A]], gf: Gen[A => M], em: Eq[M]): Property =
    Property.forAll(laws.traverseFoldMap(_: F[A], _: A => M))

  def traverseFoldLeft[A, B](implicit gfa: Gen[F[A]], gb: Gen[B], gf: Gen[(B, A) => B], eb: Eq[B]): Property =
    Property.forAll(laws.traverseFoldLeft(_: F[A], _: B, _: (B, A) => B))

  def traverseFoldRight[A, B](implicit gfa: Gen[F[A]],
                              glb: Gen[Eval[B]],
                              gf: Gen[(A, Eval[B]) => Eval[B]],
                              eb: Eq[B]): Property =
    Property.forAll(laws.traverseFoldRight(_: F[A], _: Eval[B], _: (A, Eval[B]) => Eval[B]))

  def traverseSequentialComposition[G[_]: Applicative, H[_]: Applicative, A, B, C](implicit gfa: Gen[F[A]],
                                                                                   gf: Gen[A => G[B]],
                                                                                   gg: Gen[B => H[C]],
                                                                                   eghfc: Eq[G[H[F[C]]]]): Property =
    Property.forAll(laws.traverseSequentialComposition(_: F[A], _: A => G[B], _: B => H[C]))

  def traverseParallelComposition[G[_]: Applicative, H[_]: Applicative, A, B](implicit gfa: Gen[F[A]],
                                                                              gf: Gen[A => G[B]],
                                                                              gg: Gen[A => H[B]],
                                                                              eghfb: Eq[(G[F[B]], H[F[B]])]): Property =
    Property.forAll(laws.traverseParallelComposition(_: F[A], _: A => G[B], _: A => H[B]))

  def props[G[_]: Applicative, H[_]: Applicative, A, B, C, M: Monoid](
    implicit gfa: Gen[F[A]],
    gb: Gen[B],
    glb: Gen[Eval[B]],
    gf: Gen[A => B],
    gf1: Gen[A => G[B]],
    gf2: Gen[(B, A) => B],
    gf3: Gen[(A, Eval[B]) => Eval[B]],
    gfm: Gen[A => M],
    gg1: Gen[B => H[C]],
    gg2: Gen[A => H[B]],
    eqm: Eq[M],
    eqb: Eq[B],
    efb: Eq[F[B]],
    eghfc: Eq[G[H[F[C]]]],
    eghfb: Eq[(G[F[B]], H[F[B]])]
  ): Properties[NekoLaw] =
    Properties.properties(NekoLaw.traverse)(
      NekoLaw.traverseIdentity -> traverseIdentity[A, B],
      NekoLaw.traverseFoldMap -> traverseFoldMap[A, M],
      NekoLaw.traverseFoldLeft -> traverseFoldLeft[A, B],
      NekoLaw.traverseFoldRight -> traverseFoldRight[A, B],
      NekoLaw.traverseSequentialComposition -> traverseSequentialComposition[G, H, A, B, C],
      NekoLaw.traverseSequentialComposition -> traverseParallelComposition[G, H, A, B]
    )

  def all[G[_]: Applicative, H[_]: Applicative, A, B, C, M: Monoid](
    implicit gfa: Gen[F[A]],
    gb: Gen[B],
    glb: Gen[Eval[B]],
    gf: Gen[A => B],
    gf1: Gen[A => G[B]],
    gf2: Gen[(B, A) => B],
    gf3: Gen[(A, Eval[B]) => Eval[B]],
    gfm: Gen[A => M],
    gg: Gen[B => C],
    gg1: Gen[B => H[C]],
    gg2: Gen[A => H[B]],
    em: Eq[M],
    eb: Eq[B],
    efa: Eq[F[A]],
    efb: Eq[F[B]],
    efc: Eq[F[C]],
    eghfc: Eq[G[H[F[C]]]],
    eghfb: Eq[(G[F[B]], H[F[B]])]
  ): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.traverseAll,
                         FoldProps[F].all[A, M],
                         FunctorProps[F].all[A, B, C],
                         props[G, H, A, B, C, M])
}

object TraverseProps {
  def apply[F[_]: Traverse]: TraverseProps[F] = new TraverseProps[F] {
    val laws: TraverseLaws[F] = TraverseLaws[F]
  }
}
