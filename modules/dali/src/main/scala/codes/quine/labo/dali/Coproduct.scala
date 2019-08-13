package codes.quine.labo
package dali

sealed trait Coproduct extends Serializable with Product

sealed trait CNil extends Coproduct

sealed trait :+:[+H, +T <: Coproduct] extends Coproduct {
  def eliminate[A](h: H => A, t: T => A): A
}

final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T] {
  def eliminate[A](h: H => A, t: T => A): A = h(head)
}

final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T] {
  def eliminate[A](h: H => A, t: T => A): A = t(tail)
}

object Coproduct {
  sealed class MkCoproduct[C <: Coproduct] {
    def apply[A](a: A)(implicit i: Inject[A, C]): C = i.inject(a)
  }

  def apply[C <: Coproduct]: MkCoproduct[C] = new MkCoproduct[C]

  trait Inject[A, C <: Coproduct] {
    def inject(a: A): C
  }

  object Inject {
    implicit def injectHead[A, C <: Coproduct]: Inject[A, A :+: C] = new Inject[A, A :+: C] {
      def inject(a: A): A :+: C = Inl(a)
    }

    implicit def injectTail[A, B, C <: Coproduct](implicit i: Inject[A, C]): Inject[A, B :+: C] =
      new Inject[A, B :+: C] {
        def inject(a: A): B :+: C = Inr(i.inject(a))
      }
  }
}
