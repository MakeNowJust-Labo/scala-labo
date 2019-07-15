package codes.quine.labo.nyan

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F]