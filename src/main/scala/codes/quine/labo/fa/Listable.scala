package codes.quine.labo
package fa

import simulacrum.typeclass

@typeclass trait Listable[T] {
  @simulacrum.noop
  def listAll: Iterable[T]
}

object Listable {
  implicit val boolean: Listable[Boolean] = new Listable[Boolean] {
    def listAll: Iterable[Boolean] = Iterable.apply(false, true)
  }
}
