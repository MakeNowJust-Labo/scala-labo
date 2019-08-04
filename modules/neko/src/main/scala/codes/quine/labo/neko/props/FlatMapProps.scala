package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait FlatMapProps[F[_]] {
  val laws: FlatMapLaws[F]
  import laws._

  def flatMapAssociativity[A, B, C](implicit gfa: Gen[F[A]],
                                    gf: Gen[A => F[B]],
                                    gg: Gen[B => F[C]],
                                    efc: Eq[F[C]]): Property =
    Property.forAll(laws.flatMapAssociativity(_: F[A], _: A => F[B], _: B => F[C]))

  def props[A, B, C](implicit gfa: Gen[F[A]],
                     gf: Gen[A => F[B]],
                     gg: Gen[B => F[C]],
                     efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.flatMap)(
      NekoLaw.flatMapAssociativity -> flatMapAssociativity[A, B, C]
    )

  def all[A, B, C](implicit gfa: Gen[F[A]],
                   gf0: Gen[A => B],
                   gg0: Gen[B => C],
                   gf: Gen[A => F[B]],
                   gg: Gen[B => F[C]],
                   gff: Gen[F[A => B]],
                   gfg: Gen[F[B => C]],
                   efa: Eq[F[A]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.flatMapAll, ApplyProps[F].all[A, B, C], props[A, B, C])
}

object FlatMapProps {
  def apply[F[_]: FlatMap]: FlatMapProps[F] = new FlatMapProps[F] {
    val laws: FlatMapLaws[F] = FlatMapLaws[F]
  }
}
