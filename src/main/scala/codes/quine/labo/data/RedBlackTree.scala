package codes.quine.labo
package data

import scala.annotation.tailrec
import neko.Ord, neko.syntax._, neko.data.Ordering._
import RedBlackTree._, Color._, Tree._
import com.github.ghik.silencer.silent

final case class RedBlackTree[A](tree: Tree[A]) {
  def add(x: A)(implicit A: Ord[A]): RedBlackTree[A] = RedBlackTree(RedBlackTree.add(x, tree))

  def remove(x: A)(implicit A: Ord[A]): RedBlackTree[A] = RedBlackTree(RedBlackTree.remove(x, tree))
}

object RedBlackTree {
  def empty[A]: RedBlackTree[A] = RedBlackTree(Empty)

  sealed trait Color

  object Color {
    case object Black extends Color
    case object Red extends Color
  }

  sealed trait Tree[+A] {
    def asRoot: Tree[A] = this match {
      case Empty            => Empty
      case Node(_, a, x, b) => Node(Black, a, x, b)
    }
  }

  object Tree {
    case object Empty extends Tree[Nothing]
    final case class Node[A](color: Color, left: Tree[A], value: A, right: Tree[A]) extends Tree[A]
  }

  @tailrec
  def member[A: Ord](x: A, t: Tree[A]): Boolean = t match {
    case Empty => false
    case Node(_, a, y, b) =>
      (x <=> y) match {
        case LT => member(x, a)
        case EQ => true
        case GT => member(x, b)
      }
  }

  def add[A: Ord](x: A, t: Tree[A]): Tree[A] = {
    def loop(t: Tree[A]): Node[A] = t match {
      case Empty => Node(Red, Empty, x, Empty)
      case Node(color, a, y, b) =>
        (x <=> y) match {
          case LT => balance(color, loop(a), y, b)
          case EQ => Node(color, a, y, b)
          case GT => balance(color, a, y, loop(b))
        }
    }

    loop(t).asRoot
  }

  def balance[A](color: Color, a: Tree[A], x: A, b: Tree[A]): Node[A] = (color, a, x, b) match {
    case (Black, Node(Red, Node(Red, a, x, b), y, c), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, Node(Red, b, y, c), z, d)) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) =>
      Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    case (color, a, x, b) => Node(color, a, x, b)
  }

  def remove[A: Ord](x: A, t: Tree[A]): Tree[A] = {
    def loop(t: Tree[A]): Tree[A] = t match {
      case Empty => Empty
      case Node(color, a, y, b) =>
        (x <=> y) match {
          case LT => removeLeft(Node(color, a, y, b))
          case EQ => fuse(a, b)
          case GT => removeRight(Node(color, a, y, b))
        }
    }

    def removeLeft(n: Node[A]): Node[A] = n match {
      case Node(_, t1 @ Node(Black, _, _, _), y, t2) => balanceLeft(loop(t1), y, t2)
      case Node(_, t1, y, t2)                        => Node(Red, loop(t1), y, t2)
    }

    @silent
    def balanceLeft(a: Tree[A], y: A, b: Tree[A]): Node[A] = (a, b) match {
      case (Node(Red, t1, x, t2), t3)   => Node(Red, Node(Black, t1, x, t2), y, t3)
      case (t1, Node(Black, t2, z, t3)) => balance(Black, t1, y, Node(Red, t2, z, t3))
      case (t1, Node(Red, Node(Black, t2, u, t3), z, Node(Black, t4, v, t5))) =>
        Node(Red, Node(Black, t1, y, t2), u, balance(Black, t3, z, Node(Red, t4, v, t5)))
    }

    def removeRight(n: Node[A]): Node[A] = n match {
      case Node(_, t1, y, t2 @ Node(Black, _, _, _)) => balanceRight(t1, y, loop(t2))
      case Node(_, t1, y, t2)                        => Node(Red, t1, y, loop(t2))
    }

    @silent
    def balanceRight(a: Tree[A], y: A, b: Tree[A]): Node[A] = (a, b) match {
      case (t1, Node(Red, t2, x, t3))   => Node(Red, t1, y, Node(Black, t2, x, t3))
      case (Node(Black, t1, z, t2), t3) => balance(Black, Node(Red, t1, z, t2), y, t3)
      case (Node(Red, Node(Black, t1, v, t2), z, Node(Black, t3, u, t4)), t5) =>
        Node(Red, balance(Black, Node(Red, t1, v, t2), z, t3), u, Node(Black, t4, y, t5))
    }

    def fuse(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
      case (Empty, t)                                        => t
      case (t, Empty)                                        => t
      case (t1 @ Node(Black, _, _, _), Node(Red, t3, y, t4)) => Node(Red, fuse(t1, t3), y, t4)
      case (Node(Red, t1, x, t2), t3 @ Node(Black, _, _, _)) => Node(Red, t1, x, fuse(t2, t3))
      case (Node(Red, t1, x, t2), Node(Red, t3, y, t4)) =>
        fuse(t2, t3) match {
          case Node(Red, s1, z, s2) => Node(Red, Node(Red, t1, x, s1), z, Node(Red, s2, y, t4))
          case s                    => Node(Red, t1, x, Node(Red, s, y, t4))
        }
      case (Node(Black, t1, x, t2), Node(Black, t3, y, t4)) =>
        fuse(t2, t3) match {
          case Node(Red, s1, z, s2) => Node(Red, Node(Black, t1, x, s1), z, Node(Black, s2, y, t4))
          case s                    => balanceLeft(t1, x, Node(Black, s, y, t4))
        }
    }

    loop(t).asRoot
  }
}
