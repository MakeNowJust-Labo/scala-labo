package codes.quine.labo
package neko
package props

import scalaprops._
import laws._, instances._

trait FoldProps[F[_]] {
  val laws: FoldLaws[F]

  def foldRightLazy[A](implicit gfa: Gen[F[A]]): Property =
    Property.forAll(laws.foldRightLazy(_: F[A]))

  def leftConstencyWithFoldMap[A, M: Monoid](implicit gfa: Gen[F[A]], gf: Gen[A => M], em: Eq[M]): Property =
    Property.forAll(laws.leftConststencyWithFoldMap(_: F[A], _: A => M))

  def rightConsistencyWithFoldMap[A, M: Monoid](implicit gfa: Gen[F[A]], gf: Gen[A => M], em: Eq[M]): Property =
    Property.forAll(laws.rightConsistencyWithFoldMap(_: F[A], _: A => M))

  def props[A, M: Monoid](implicit gfa: Gen[F[A]], gf: Gen[A => M], em: Eq[M]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.fold)(
      NekoLaw.foldFoldRightLazy -> foldRightLazy[A],
      NekoLaw.foldLeftConsistencyWithFoldMap -> leftConstencyWithFoldMap[A, M],
      NekoLaw.foldRightConsistencyWithFoldMap -> rightConsistencyWithFoldMap[A, M]
    )

  def all[A, M: Monoid](implicit gfa: Gen[F[A]], gf: Gen[A => M], em: Eq[M]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.foldAll, props[A, M])
}

object FoldProps {
  def apply[F[_]: Fold]: FoldProps[F] = new FoldProps[F] {
    val laws: FoldLaws[F] = FoldLaws[F]
  }
}
