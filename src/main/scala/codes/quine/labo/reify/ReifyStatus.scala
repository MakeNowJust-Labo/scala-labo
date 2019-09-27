package codes.quine.labo
package reify

import neko._, data._

private[reify] case class ReifyStatus[D[_]](visited: IdentityMap[Any, Unique],
                                            nodes: Map[Unique, D[Unique]],
                                            unique: Unique)

object ReifyStatus {
  def empty[D[_]]: ReifyStatus[D] =
    ReifyStatus(IdentityMap.empty, Map.empty, Unique.zero)

  def nextUnique[D[_]]: State[ReifyStatus[D], Unique] =
    for {
      s <- State.get
      u = s.unique
      _ <- State.put(s.copy(unique = u.next))
    } yield u

  def lookupVisited[D[_]](a: Any): State[ReifyStatus[D], Option[Unique]] =
    State.get.map(_.visited.get(a))

  def putVisited[D[_]](a: Any): State[ReifyStatus[D], Unique] =
    for {
      u <- nextUnique[D]
      s <- State.get
      v = s.visited
      _ <- State.put(s.copy(visited = v.updated(a, u)))
    } yield u

  def putNodes[D[_]](u: Unique, r: D[Unique]): State[ReifyStatus[D], Unit] =
    for {
      s <- State.get
      n = s.nodes
      _ <- State.put(s.copy(nodes = n.updated(u, r)))
    } yield ()
}
