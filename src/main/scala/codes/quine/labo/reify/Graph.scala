package codes.quine.labo
package reify

import neko._, neko.data._, neko.instances._, neko.syntax._
import neko.rec._

case class Graph[F[_]](nodes: Map[Unique, F[Unique]], root: Unique) {
  def reflect[T](implicit FT: Corecursive[F, T], T: Traverse[F]): Option[T] =
    StateT.eval(reflectNodes[T](root))(Reflect.empty(nodes))

  private[this] def reflectNodes[T](u: Unique)(implicit FT: Corecursive[F, T], F: Traverse[F]): Reflect.State[F, T, T] =
    Reflect.lookupCache[F, T](u) <+>
      (for {
        fu <- Reflect.lookupNodes(u)
        ft <- fu.traverse(reflectNodes(_))
        t = FT.embed(ft)
        _ <- Reflect.putCache(u, t)
      } yield t)
}

object Graph {
  def reify[F[_], T](t: T)(implicit FT: Recursive[F, T], F: Traverse[F]): Graph[F] = {
    val (s, u) = State.run(reifyNodes[F, T](t))(Reify.empty)
    Graph(s.nodes, u)
  }

  private[this] def reifyNodes[F[_]: Traverse, T](t: T)(implicit FT: Recursive[F, T]): Reify.State[F, T, Unique] =
    Reify.lookupVisited(t).flatMap {
      case Some(u) => Reify.monad.pure(u)
      case None =>
        for {
          u <- Reify.putVisited(t)
          r <- FT.project(t).traverse(reifyNodes[F, T](_))
          _ <- Reify.putNodes(u, r)
        } yield u
    }
}
