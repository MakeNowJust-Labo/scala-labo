package codes.quine.labo
package reify

import neko._

trait MuRef[A] {
  type DeRef[_]

  def mapDeRef[F[_]: Applicative, R](a: A)(f: MuRefFunction[F, DeRef, R]): F[DeRef[R]]
}

object MuRef {
  type Aux[A, D[_]] = MuRef[A] { type DeRef[B] = D[B] }
}

/**
 * MuRefFunction represents a function that takes some A value with MuRef.Aux[A, D] then returns F[R].
 *
 * Scala cannot represent such type as Function1 for now due to two reasons:
 *   1) This function's argument is polymorphic.
 *   2) This functions takes an implicit parameter.
 *
 * It is the second argument of [[MuRef#mapDeRef]].
 */
trait MuRefFunction[F[_], D[_], R] {
  def apply[A](a: A)(implicit A: MuRef.Aux[A, D]): F[R]
}
