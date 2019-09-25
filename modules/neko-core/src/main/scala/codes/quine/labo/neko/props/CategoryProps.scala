package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait CategoryProps[F[_, _]] {
  val laws: CategoryLaws[F]
  import laws._

  def categoryLeftIdentity[A, B](implicit gfab: Gen[F[A, B]], efab: Eq[F[A, B]]): Property =
    Property.forAll(laws.categoryLeftIdentity(_: F[A, B]))

  def categoryRightIdentity[A, B](implicit gfab: Gen[F[A, B]], efab: Eq[F[A, B]]): Property =
    Property.forAll(laws.categoryRightIdentity(_: F[A, B]))

  def props[A, B](implicit gfab: Gen[F[A, B]], efab: Eq[F[A, B]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.category)(
      NekoLaw.categoryLeftIdentity -> categoryLeftIdentity[A, B],
      NekoLaw.categoryRightIdentity -> categoryRightIdentity[A, B]
    )

  def all[A, B, C, D](implicit gfab: Gen[F[A, B]],
                      gfbc: Gen[F[B, C]],
                      gfcd: Gen[F[C, D]],
                      efab: Eq[F[A, B]],
                      efad: Eq[F[A, D]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.categoryAll, ComposeProps[F].all[A, B, C, D], props[A, B])
}

object CategoryProps {
  def apply[F[_, _]: Category]: CategoryProps[F] = new CategoryProps[F] {
    val laws: CategoryLaws[F] = CategoryLaws[F]
  }
}
