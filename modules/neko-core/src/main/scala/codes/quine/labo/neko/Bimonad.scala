package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Bimonad[F[_]] extends Monad[F] with Comonad[F]
