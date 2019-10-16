package codes.quine.labo

package object hollow {
  type ~[+A, +B] = (A, B)

  object ~ {
    def unapply[A, B](ab: A ~ B): Option[(A, B)] = Some(ab)
  }
}
