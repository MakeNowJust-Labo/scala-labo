package codes.quine.labo
package fa

import TWDFA._, convert._, TWDFA2DFA._
import scalaprops._

object TestTWDFA {
  sealed trait Q
  case object q0 extends Q
  case object q1 extends Q
  case object q2 extends Q
  case object p0 extends Q
  case object p1 extends Q
  case object t extends Q
  case object r extends Q

  object Q {
    implicit val listable: Listable[Q] = new Listable[Q] {
      def listAll: Iterable[Q] = Iterable(q0, q1, q2, p0, p1, t, r)
    }
    implicit val gen: Gen[Q] = Gen.elements(q0, q1, q2, p0, p1, t, r)
  }

  sealed trait Σ
  case object a extends Σ
  case object b extends Σ

  object Σ {
    implicit val listable: Listable[Σ] = new Listable[Σ] {
      def listAll: Iterable[Σ] = Iterable(a, b)
    }
    implicit val gen: Gen[Σ] = Gen.elements(a, b)
  }

  val M: TWDFA[Q, Σ] = TWDFA(
    Map(
      // |-
      ((q0, Bounded.Begin), (q0, Direction.Right)),
      ((q1, Bounded.Begin), (r, Direction.Right)),
      ((q2, Bounded.Begin), (r, Direction.Right)),
      ((p0, Bounded.Begin), (t, Direction.Right)),
      ((p1, Bounded.Begin), (r, Direction.Right)),
      ((t, Bounded.Begin), (t, Direction.Right)),
      ((r, Bounded.Begin), (r, Direction.Right)),
      // a
      ((q0, Bounded.Alphabet(a)), (q1, Direction.Right)),
      ((q1, Bounded.Alphabet(a)), (q2, Direction.Right)),
      ((q2, Bounded.Alphabet(a)), (q0, Direction.Right)),
      ((p0, Bounded.Alphabet(a)), (p0, Direction.Left)),
      ((p1, Bounded.Alphabet(a)), (p1, Direction.Left)),
      ((t, Bounded.Alphabet(a)), (t, Direction.Right)),
      ((r, Bounded.Alphabet(a)), (r, Direction.Right)),
      // b
      ((q0, Bounded.Alphabet(b)), (q0, Direction.Right)),
      ((q1, Bounded.Alphabet(b)), (q1, Direction.Right)),
      ((q2, Bounded.Alphabet(b)), (q2, Direction.Right)),
      ((p0, Bounded.Alphabet(b)), (p1, Direction.Left)),
      ((p1, Bounded.Alphabet(b)), (p0, Direction.Left)),
      ((t, Bounded.Alphabet(b)), (t, Direction.Right)),
      ((r, Bounded.Alphabet(b)), (r, Direction.Right)),
      // -|
      ((q0, Bounded.End), (p0, Direction.Left)),
      ((q1, Bounded.End), (r, Direction.Left)),
      ((q2, Bounded.End), (r, Direction.Left)),
      ((p0, Bounded.End), (r, Direction.Left)),
      ((p1, Bounded.End), (r, Direction.Left)),
      ((t, Bounded.End), (t, Direction.Left)),
      ((r, Bounded.End), (r, Direction.Left))
    ),
    q0,
    t,
    r
  )

  val D: DFA[Table[Q], Σ] = TWDFA2DFA(M)
}
