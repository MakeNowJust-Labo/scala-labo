package codes.quine.labo
package neko
package laws

import data._, syntax._, instances.list._

trait TraverseLaws[F[_]] {
  implicit val F: Traverse[F]

  def traverseIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.map(f) <-> fa.traverse[Id, B](a => Id(f(a))).value

  def traverseFoldMap[A, M: Monoid](fa: F[A], f: A => M): IsEq[M] =
    fa.foldMap(f) <-> fa.traverse(a => Const[M, M](f(a))).value

  def traverseFoldLeft[A, B](fa: F[A], b: B, f: (B, A) => B): IsEq[B] =
    fa.foldLeft(b)(f) <-> fa.foldMap(a => List(a)).foldLeft(b)(f)

  def traverseFoldRight[A, B](fa: F[A], lb: Eval[B], f: (A, Eval[B]) => Eval[B]): IsEq[Eval[B]] =
    fa.foldRight(lb)(f) <-> fa.foldMap(a => Endo.lift[Eval[B]](lb => f(a, lb)))(MonoidK[Endo].algebra).apply(lb)

  def traverseSequentialComposition[G[_], H[_], A, B, C](fa: F[A], f: A => G[B], g: B => H[C])(
    implicit G: Applicative[G],
    H: Applicative[H]
  ): IsEq[Nested[G, H, F[C]]] = {
    val lhs = Nested(fa.traverse(f).map(_.traverse(g)))
    val rhs = fa.traverse[Nested[G, H, *], C](a => Nested(f(a).map(g)))
    lhs <-> rhs
  }

  def traverseParallelComposition[G[_], H[_], A, B](fa: F[A], f: A => G[B], g: A => H[B])(
    implicit G: Applicative[G],
    H: Applicative[H]
  ): IsEq[(G[F[B]], H[F[B]])] = {
    type GH[B0] = (G[B0], H[B0])
    implicit val GH = new Applicative[GH] {
      def pure[A0](a: A0): GH[A0] = (G.pure(a), H.pure(a))
      def ap[A0, B0](ff: GH[A0 => B0])(fa: GH[A0]): GH[B0] = {
        val (gf, hf) = ff
        val (ga, ha) = fa
        (gf <*> ga, hf <*> ha)
      }
    }

    val lhs = fa.traverse[GH, B](a => (f(a), g(a)))
    val rhs = (fa.traverse(f), fa.traverse(g))
    lhs <-> rhs
  }
}

object TraverseLaws {
  def apply[F[_]: Traverse]: TraverseLaws[F] = new TraverseLaws[F] {
    val F: Traverse[F] = Traverse[F]
  }
}
