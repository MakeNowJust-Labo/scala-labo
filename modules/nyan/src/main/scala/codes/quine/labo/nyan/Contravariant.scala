package codes.quine.labo.nyan

import simulacrum.typeclass

@typeclass trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}
