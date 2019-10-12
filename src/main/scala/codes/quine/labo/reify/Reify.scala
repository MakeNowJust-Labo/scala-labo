package codes.quine.labo
package reify

import neko._

private[reify] case class Reify[F[_], T](visited: IdentityMap[T, Unique], nodes: Map[Unique, F[Unique]], unique: Unique)

object Reify {
  def empty[F[_], T]: Reify[F, T] =
    Reify(IdentityMap.empty, Map.empty, Unique.zero)

  type State[F[_], T, A] = data.State[Reify[F, T], A]
  def monad[F[_], T]: Monad[State[F, T, *]] = implicitly
  def state[F[_], T]: MonadState[State[F, T, *], Reify[F, T]] = implicitly

  def nextUnique[F[_], T]: State[F, T, Unique] =
    for {
      s <- state.get
      u = s.unique
      _ <- state.put(s.copy(unique = u.next))
    } yield u

  def lookupVisited[F[_], T](t: T): State[F, T, Option[Unique]] =
    state.get.map(_.visited.get(t))

  def putVisited[F[_], T](t: T): State[F, T, Unique] =
    for {
      u <- nextUnique
      s <- state.get
      v = s.visited
      _ <- state.put(s.copy(visited = v.updated(t, u)))
    } yield u

  def putNodes[F[_], T](u: Unique, r: F[Unique]): State[F, T, Unit] =
    for {
      s <- state.get
      n = s.nodes
      _ <- state.put(s.copy(nodes = n.updated(u, r)))
    } yield ()
}
