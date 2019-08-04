package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait CoflatMapProps[F[_]] {
  val laws: CoflatMapLaws[F]
  import laws._

  def coflatMapAssociativity[A, B, C](implicit gfa: Gen[F[A]],
                                      gf: Gen[F[A] => B],
                                      gg: Gen[F[B] => C],
                                      efc: Eq[F[C]]): Property =
    Property.forAll(laws.coflatMapAssociativity(_: F[A], _: F[A] => B, _: F[B] => C))

  def coflatMapCoherence[A, B](implicit gfa: Gen[F[A]], gf: Gen[F[A] => B], efb: Eq[F[B]]): Property =
    Property.forAll(laws.coflatMapCoherence(_: F[A], _: F[A] => B))

  def coflatMapIdentity[A](implicit gfa: Gen[F[A]], effa: Eq[F[F[A]]]): Property =
    Property.forAll(laws.coflatMapIdentity(_: F[A]))

  def props[A, B, C](implicit gfa: Gen[F[A]],
                     gf: Gen[F[A] => B],
                     gg: Gen[F[B] => C],
                     effa: Eq[F[F[A]]],
                     efb: Eq[F[B]],
                     efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.coflatMap)(
      NekoLaw.coflatMapAssociativity -> coflatMapAssociativity[A, B, C],
      NekoLaw.coflatMapCoherence -> coflatMapCoherence[A, B],
      NekoLaw.coflatMapIdentity -> coflatMapIdentity[A]
    )

  def all[A, B, C](implicit gfa: Gen[F[A]],
                   gf0: Gen[A => B],
                   gg0: Gen[B => C],
                   gf: Gen[F[A] => B],
                   gg: Gen[F[B] => C],
                   efa: Eq[F[A]],
                   effa: Eq[F[F[A]]],
                   efb: Eq[F[B]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.coflatMapAll, FunctorProps[F].all[A, B, C], props[A, B, C])
}

object CoflatMapProps {
  def apply[F[_]: CoflatMap]: CoflatMapProps[F] = new CoflatMapProps[F] {
    val laws: CoflatMapLaws[F] = CoflatMapLaws[F]
  }
}
