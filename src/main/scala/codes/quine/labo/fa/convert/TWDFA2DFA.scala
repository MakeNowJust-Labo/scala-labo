package codes.quine.labo
package fa
package convert

import scala.annotation.tailrec
import scala.collection.mutable

import TWDFA._

object TWDFA2DFA {
  final case class Table[Q](value: Map[Input[Q], Output[Q]])

  sealed trait Input[+Q]

  object Input {
    case object Forward extends Input[Nothing]
    final case class Backward[Q](q: Q) extends Input[Q]
  }

  sealed trait Output[+Q]

  object Output {
    case object Loop extends Output[Nothing]
    final case class Next[Q](q: Q) extends Output[Q]
  }

  def apply[Q: Listable, A: Listable](m: TWDFA[Q, A]): DFA[Table[Q], A] = {
    val TWDFA(trans, start, accept, reject) = m

    val startTable = {
      val builder = Map.newBuilder[Input[Q], Output[Q]]
      for (q <- Listable[Q].listAll) {
        builder.addOne((Input.Backward(q), Output.Next((trans((q, Bounded.Begin))._1))))
      }
      builder.addOne((Input.Forward, Output.Next(trans((start, Bounded.Begin))._1)))
      Table(builder.result())
    }

    val transTable = mutable.Map.empty[(Table[Q], A), Table[Q]]

    def buildTrans(t: Table[Q], a: A): Table[Q] = {
      @tailrec def loop(visited: Set[Q], q: Q): Output[Q] =
        if (visited.contains(q)) Output.Loop
        else
          trans((q, Bounded.Alphabet(a))) match {
            case (q1, Direction.Right) => Output.Next(q1)
            case (q1, Direction.Left) =>
              t.value(Input.Backward(q1)) match {
                case Output.Loop     => Output.Loop
                case Output.Next(q2) => loop(visited + q, q2)
              }
          }

      val builder = Map.newBuilder[Input[Q], Output[Q]]
      for (q <- Listable[Q].listAll) {
        builder.addOne((Input.Backward(q), loop(Set.empty, q)))
      }
      t.value(Input.Forward) match {
        case Output.Loop    => builder.addOne((Input.Forward, Output.Loop))
        case Output.Next(q) => builder.addOne((Input.Forward, loop(Set.empty, q)))
      }
      val s = Table(builder.result())
      transTable.put((t, a), s)
      s
    }

    val added = mutable.Set.empty[Table[Q]]
    val queue = mutable.Queue.empty[Table[Q]]
    queue.enqueue(startTable)
    added += startTable
    while (queue.nonEmpty) {
      val t = queue.dequeue
      for (a <- Listable[A].listAll) {
        val s = buildTrans(t, a)
        if (!added.contains(s)) {
          queue.enqueue(s)
          added += s
        }
      }
    }

    def isAccept(t: Table[Q]): Boolean = {
      @tailrec def loop(visited: Set[Q], o: Output[Q]): Boolean =
        o match {
          case Output.Loop                           => false
          case Output.Next(q) if visited.contains(q) => false
          case Output.Next(q) if q == accept         => true
          case Output.Next(q) =>
            trans((q, Bounded.End)) match {
              case (q1, Direction.Left) => loop(visited + q, t.value(Input.Backward(q1)))
            }
        }

      loop(Set.empty, t.value(Input.Forward))
    }

    val accepts = added.filter(isAccept(_))

    DFA(transTable.toMap, startTable, accepts.toSet)
  }
}
