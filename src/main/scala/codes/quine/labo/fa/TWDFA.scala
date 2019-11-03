package codes.quine.labo
package fa

import scala.annotation.tailrec

import TWDFA._

/**
 * TWDFA represents "Two-Way Deterministic Finite Automata".
 *
 * @see [[https://en.wikipedia.org/wiki/Two-way_finite_automaton]]
 */
final case class TWDFA[Q, A](trans: Map[(Q, Bounded[A]), (Q, Direction)], start: Q, accept: Q, reject: Q) {
  def run(xs: List[A]): Boolean = {
    val bxs = Bounded.Begin +: xs.map(Bounded.Alphabet(_)).toVector :+ Bounded.End
    @tailrec def loop(visited: Set[(Q, Int)], q: Q, i: Int): Boolean =
      if (q == accept) true
      else if (q == reject || visited.contains((q, i))) false
      else
        trans.get((q, bxs(i))) match {
          case Some((q1, Direction.Left))  => loop(visited.incl((q, i)), q1, i - 1)
          case Some((q1, Direction.Right)) => loop(visited.incl((q, i)), q1, i + 1)
          case None                        => false
        }
    loop(Set.empty, start, 0)
  }
}

object TWDFA {
  sealed trait Direction extends Serializable with Product

  object Direction {
    case object Left extends Direction
    case object Right extends Direction
  }

  sealed trait Bounded[+A] extends Serializable with Product

  object Bounded {

    /** Begin marker of input list. */
    case object Begin extends Bounded[Nothing]

    /** End marker of input list. */
    case object End extends Bounded[Nothing]

    /** Alphabet element of input list. */
    final case class Alphabet[+A](a: A) extends Bounded[A]
  }
}
