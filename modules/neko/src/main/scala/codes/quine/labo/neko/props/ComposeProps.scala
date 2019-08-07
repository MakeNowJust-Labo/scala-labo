package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait ComposeProps[F[_, _]] {
  val laws: ComposeLaws[F]

  def composeAssociativity[A, B, C, D](implicit gfab: Gen[F[A, B]],
                                       gfbc: Gen[F[B, C]],
                                       gfcd: Gen[F[C, D]],
                                       efad: Eq[F[A, D]]): Property =
    Property.forAll(laws.composeAssociativity(_: F[A, B], _: F[B, C], _: F[C, D]))

  def props[A, B, C, D](implicit gfab: Gen[F[A, B]],
                        gfbc: Gen[F[B, C]],
                        gfcd: Gen[F[C, D]],
                        efad: Eq[F[A, D]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.compose)(
      NekoLaw.composeAssociativity -> composeAssociativity[A, B, C, D]
    )

  def all[A, B, C, D](implicit gfab: Gen[F[A, B]],
                      gfbc: Gen[F[B, C]],
                      gfcd: Gen[F[C, D]],
                      efad: Eq[F[A, D]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.composeAll, props[A, B, C, D])
}

object ComposeProps {
  def apply[F[_, _]: Compose]: ComposeProps[F] = new ComposeProps[F] {
    val laws: ComposeLaws[F] = ComposeLaws[F]
  }
}
