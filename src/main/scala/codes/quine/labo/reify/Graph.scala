package codes.quine.labo
package reify

import neko._, data._

case class Graph[F[_]](nodes: Map[Unique, F[Unique]], root: Unique)

object Graph {
  def reify[A, D[_]](a: A)(implicit A: MuRef.Aux[A, D]): Graph[D] = {
    val (status, root) = State.run(findNodes(a))(ReifyStatus.empty)
    Graph(status.nodes, root)
  }

  private def findNodes[D[_]]: MuRefFunction[State[ReifyStatus[D], *], D, Unique] =
    new MuRefFunction[State[ReifyStatus[D], *], D, Unique] {
      def apply[A](a: A)(implicit A: MuRef.Aux[A, D]): State[ReifyStatus[D], Unique] =
        ReifyStatus.lookupVisited(a).flatMap {
          case Some(u) => State.pure[ReifyStatus[D], Unique](u)
          case None =>
            for {
              u <- ReifyStatus.putVisited(a)
              r <- A.mapDeRef(a)(findNodes)
              _ <- ReifyStatus.putNodes(u, r)
            } yield u
        }
    }
}
