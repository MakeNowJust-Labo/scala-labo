package codes.quine.labo
package fa

/**
 * DFA represents "Deterministic Finite Automata".
 *
 * @see [[https://en.wikipedia.org/wiki/Deterministic_finite_automaton]]
 */
final case class DFA[Q, A](trans: Map[(Q, A), Q], start: Q, accepts: Set[Q]) {
  def run(as: List[A]): Boolean =
    as.foldLeft(Option(start)) { case (oq, a) => oq.flatMap(q => trans.get((q, a))) }
      .exists(accepts.contains(_))
}
