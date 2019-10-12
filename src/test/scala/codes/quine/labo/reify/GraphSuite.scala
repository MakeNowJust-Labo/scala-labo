package codes.quine.labo
package reify

import minitest._

object GraphSuite extends SimpleTestSuite {
  import TermF._

  test("""Graph.reify(p && p)""") {
    val p = Var("p")
    val t = p && p
    val r = Graph.reify(t)
    val g = Graph(Map(Unique(0) -> AndF(Unique(1), Unique(1)), Unique(1) -> VarF[Unique]("p")), Unique(0))
    assertEquals(r, g)

    val ot = r.reflect[Term]
    assertEquals(ot, Some(t))
    assertEquals(Graph.reify(ot.get), g)
  }

  test("""Graph.reify((p && q) <-> ~(~p || ~q))""") {
    val p = Var("p")
    val q = Var("q")
    val t = (p && q) <-> ~(~p || ~q)
    val r = Graph.reify(t)
    val g = Graph(
      Map(
        Unique(0) -> AndF(Unique(1), Unique(9)),
        Unique(1) -> ImplyF(Unique(2), Unique(5)),
        Unique(2) -> AndF(Unique(3), Unique(4)),
        Unique(3) -> VarF[Unique]("p"),
        Unique(4) -> VarF[Unique]("q"),
        Unique(5) -> NotF(Unique(6)),
        Unique(6) -> OrF(Unique(7), Unique(8)),
        Unique(7) -> NotF(Unique(3)),
        Unique(8) -> NotF(Unique(4)),
        Unique(9) -> ImplyF(Unique(5), Unique(2))
      ),
      Unique(0)
    )
    assertEquals(r, g)

    val ot = r.reflect[Term]
    assertEquals(ot, Some(t))
    assertEquals(Graph.reify(ot.get), g)
  }
}
