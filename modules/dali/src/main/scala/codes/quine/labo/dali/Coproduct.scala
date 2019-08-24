package codes.quine.labo
package dali

import scala.annotation.tailrec

sealed trait Coproduct extends Serializable with Product

sealed trait CNil extends Coproduct

sealed trait :+:[+H, +T <: Coproduct] extends Coproduct

final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T]

final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T]

object Coproduct {
  def unsafeApply(n: Int, v: Any): Coproduct =
    (0 until n).foldLeft[Coproduct](Inl(v))((acc, _) => Inr(acc))

  @tailrec
  def unsafeGet(c: Coproduct): Any = c match {
    case Inl(h) => h
    case Inr(t) => unsafeGet(t)
  }

  sealed class Apply[C <: Coproduct] {
    def apply[A](a: A)(implicit i: Inject[A, C]): C = i.inject(a)
  }

  def apply[C <: Coproduct]: Apply[C] = new Apply[C]

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
