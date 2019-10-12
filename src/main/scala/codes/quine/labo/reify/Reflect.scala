package codes.quine.labo
package reify

import neko._, neko.instances._

final private[reify] case class Reflect[F[_], T](nodes: Map[Unique, F[Unique]], cache: Map[Unique, T])

object Reflect {
  def empty[F[_], T](nodes: Map[Unique, F[Unique]]): Reflect[F, T] = Reflect(nodes, Map.empty)

  type State[F[_], T, A] = data.StateT[Option, Reflect[F, T], A]
  def state[F[_], T]: MonadState[State[F, T, *], Reflect[F, T]] = implicitly
  def trans[F[_], T]: MonadTrans[State[F, T, *], Option] = implicitly

  def lookupNodes[F[_], T](u: Unique): State[F, T, F[Unique]] =
    state.get.flatMap(s => trans.lift(s.nodes.get(u)))

  def lookupCache[F[_], T](u: Unique): State[F, T, T] =
    state.get.flatMap(s => trans.lift(s.cache.get(u)))

  def putCache[F[_], T](u: Unique, t: T): State[F, T, Unit] =
    state.modify(s => s.copy(cache = s.cache.updated(u, t)))
}
