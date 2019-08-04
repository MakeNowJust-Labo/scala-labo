package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait ApplyProps[F[_]] {
  val laws: ApplyLaws[F]
  import laws._

  def applyComposition[A, B, C](implicit gfa: Gen[F[A]],
                                gff: Gen[F[A => B]],
                                gfg: Gen[F[B => C]],
                                efc: Eq[F[C]]): Property =
    Property.forAll(laws.applyComposition(_: F[A], _: F[A => B], _: F[B => C]))

  def props[A, B, C](implicit gfa: Gen[F[A]],
                     gff: Gen[F[A => B]],
                     gfg: Gen[F[B => C]],
                     efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties
      .properties(NekoLaw.apply)(
        NekoLaw.applyComposition -> applyComposition[A, B, C]
      )
      .andThenParam(Param.maxSize(20))

  def all[A, B, C](implicit gf: Gen[A => B],
                   gg: Gen[B => C],
                   gfa: Gen[F[A]],
                   gff: Gen[F[A => B]],
                   gfg: Gen[F[B => C]],
                   efa: Eq[F[A]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.applyAll, FunctorProps[F].all[A, B, C], props[A, B, C])
}

object ApplyProps {
  def apply[F[_]: Apply]: ApplyProps[F] = new ApplyProps[F] {
    val laws: ApplyLaws[F] = ApplyLaws[F]
  }
}
