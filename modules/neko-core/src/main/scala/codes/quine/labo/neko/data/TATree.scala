package codes.quine.labo
package neko
package data

import scala.annotation.tailrec

/**
 * TATree is a data structure represents type-aligned sequence.
 * Generally it is free Category with binary tree encoding.
 *
 * It is used for stack-safe function composition.
 *
 * @see [[http://hackage.haskell.org/package/type-aligned]]
 */
sealed trait TATree[F[_, _], -A, +B] extends Serializable with Product { f =>
  def andThen[C](g: TATree[F, B, C]): TATree[F, A, C] = TATree.andThen(f, g)

  def compose[C](g: TATree[F, C, A]): TATree[F, C, B] = TATree.andThen(g, f)

  def view: TATree.View[F, A, B] = TATree.view(f)
}

object TATree extends TATreeInstances0 {
  def lift[F[_, _], A, B](f: F[A, B]): TATree[F, A, B] = Leaf(f)

  def identity[F[_, _], A]: TATree[F, A, A] = Empty[F, A]

  final case class Empty[F[_, _], A]() extends TATree[F, A, A]

  object Empty {
    private[this] val instance: Empty[Lambda[(A, B) => Any], Any] = new Empty
    def apply[F[_, _], A]: Empty[F, A] = instance.asInstanceOf[Empty[F, A]]
  }

  final case class Leaf[F[_, _], A, B](f: F[A, B]) extends TATree[F, A, B]

  final case class Node[F[_, _], A, B, C](l: TATree[F, A, B], r: TATree[F, B, C]) extends TATree[F, A, C]

  sealed trait View[F[_, _], -A, +B] extends Serializable with Product

  object View {
    final case class Stop[F[_, _], A]() extends View[F, A, A]

    object Stop {
      private[this] val instance: Stop[Lambda[(A, B) => Any], Any] = new Stop
      def apply[F[_, _], A]: Stop[F, A] = instance.asInstanceOf[Stop[F, A]]
    }

    final case class Continue[F[_, _], A, B, C](f: F[A, B], k: TATree[F, B, C]) extends View[F, A, C]
  }

  private[data] def andThen[F[_, _], A, B, C](f: TATree[F, A, B], g: TATree[F, B, C]): TATree[F, A, C] =
    (f, g) match {
      case (_: Empty[F, A], g) => g
      case (f, _: Empty[F, B]) => f
      case _                   => Node(f, g)
    }

  private[data] def view[F[_, _], A, B](f: TATree[F, A, B]): View[F, A, B] =
    f match {
      case _: Empty[F, A] => View.Stop[F, A]
      case Leaf(f)        => View.Continue(f, TATree.identity[F, B])
      case Node(l, r) =>
        @tailrec def loop[A1, B1, C1](l: TATree[F, A1, B1], r: TATree[F, B1, C1]): View[F, A1, C1] =
          l match {
            case _: Empty[F, A1] => loop(r.asInstanceOf[TATree[F, A1, C1]], TATree.identity[F, C1])
            case Leaf(f)         => View.Continue(f, r)
            case Node(ll, lr)    => loop(ll, lr.andThen(r))
          }
        loop(l, r)
    }
}

private[data] trait TATreeInstances0 {
  implicit def tatreeCategory[F[_, _]]: Category[TATree[F, *, *]] = new Category[TATree[F, *, *]] {
    def identity[A]: TATree[F, A, A] = TATree.identity
    def compose[A, B, C](f: TATree[F, B, C], g: TATree[F, A, B]): TATree[F, A, C] = f.compose(g)
    override def andThen[A, B, C](f: TATree[F, A, B], g: TATree[F, B, C]): TATree[F, A, C] = f.andThen(g)
  }
}
