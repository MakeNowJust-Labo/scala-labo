package codes.quine.labo
package neko
package data

import scala.annotation.tailrec

final case class AndThen[-A, +B](f: TATree[Function1, A, B]) extends Function1[A, B] {
  def apply(a: A): B = AndThen.evaluate(f, a)

  override def andThen[C](g: B => C): AndThen[A, C] = g match {
    case AndThen(gf) => AndThen(f.andThen(gf))
    case gf          => AndThen(f.andThen(TATree.lift(gf)))
  }

  override def compose[C](g: C => A): AndThen[C, B] = g match {
    case AndThen(gf) => AndThen(f.compose(gf))
    case gf          => AndThen(f.compose(TATree.lift(gf)))
  }
}

object AndThen extends AndThenInstances0 {
  import TATree._

  def identity[A]: AndThen[A, A] = AndThen(TATree.identity)

  def lift[A, B](f: A => B): AndThen[A, B] = AndThen(TATree.lift(f))

  @tailrec
  private[data] def evaluate[A, B](f: TATree[Function1, A, B], a: A): B =
    f.view match {
      case _: View.Stop[Function1, A] => a
      case View.Continue(f, k)        => evaluate(k, f(a))
    }
}

trait AndThenInstances0 {
  implicit val andThenArrowInstance: Arrow[AndThen] = new Arrow[AndThen] {
    def identity[A]: AndThen[A, A] = AndThen.identity
    def compose[A, B, C](f: AndThen[B, C], g: AndThen[A, B]): AndThen[A, C] = f.compose(g)
    override def andThen[A, B, C](f: AndThen[A, B], g: AndThen[B, C]): AndThen[A, C] = f.andThen(g)

    def lift[A, B](f: A => B): AndThen[A, B] = AndThen.lift(f)

    def first[A, B, C](f: AndThen[A, B]): AndThen[(A, C), (B, C)] = AndThen.lift { case (a, c)           => (f(a), c) }
    override def second[A, B, C](f: AndThen[A, B]): AndThen[(C, A), (C, B)] = AndThen.lift { case (c, a) => (c, f(a)) }
    override def split[A, B, C, D](f: AndThen[A, B], g: AndThen[C, D]): AndThen[(A, C), (B, D)] = AndThen.lift {
      case (a, b) => (f(a), g(b))
    }
    override def merge[A, B, C](f: AndThen[A, B], g: AndThen[A, C]): AndThen[A, (B, C)] = AndThen.lift { a =>
      (f(a), g(a))
    }
  }
}
