package codes.quine.labo
package neko
package data

final case class Const[A, B](value: A)

object Const {
  def ConstInstances[A]: Functor[Const[A, *]] = new Functor[Const[A, *]] {
    def map[B, C](fa: Const[A, B])(f: B => C): Const[A, C] = Const(fa.value)
  }
}
