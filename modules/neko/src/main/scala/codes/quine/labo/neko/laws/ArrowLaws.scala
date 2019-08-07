package codes.quine.labo
package neko
package laws

import instances.function1._, syntax._

trait ArrowLaws[F[_, _]] {
  implicit val F: Arrow[F]

  def arrowIdentity[A]: IsEq[F[A, A]] =
    F.lift(identity[A]) <-> F.identity

  def arrowComposition[A, B, C](f: A => B, g: B => C): IsEq[F[A, C]] =
    F.lift(f).andThen(F.lift(g)) <-> F.lift(f.andThen(g))

  def arrowExtension[A, B, C](f: A => B): IsEq[F[(A, C), (B, C)]] =
    F.lift(f).first[C] <-> F.lift(f.split(identity))

  def arrowFunctor[A, B, C, D](fab: F[A, B], fbc: F[B, C]): IsEq[F[(A, D), (C, D)]] =
    fab.andThen(fbc).first[D] <-> fab.first[D].andThen(fbc.first[D])

  def arrowExchange[A, B, C, D](fab: F[A, B], f: C => D): IsEq[F[(A, C), (B, D)]] =
    fab.first[C].andThen(F.lift((identity[B] _) *** f)) <-> F.lift((identity[A] _) *** f).andThen(fab.first[D])

  def arrowUnit[A, B, C](fab: F[A, B]): IsEq[F[(A, C), B]] =
    fab.first[C].andThen(F.lift((_: (B, C))._1)) <-> F.lift((_: (A, C))._1).andThen(fab)

  private def assoc[A, B, C]: (((A, B), C)) => (A, (B, C)) = { case ((a, b), c) => (a, (b, c)) }

  def arrowAssociativity[A, B, C, D](fab: F[A, B]): IsEq[F[((A, C), D), (B, (C, D))]] =
    fab.first[C].first[D].andThen(F.lift(assoc)) <-> F.lift(assoc[A, C, D]).andThen(fab.first)

  def arrowSplitConsistency[A, B, C, D](fab: F[A, B], fcd: F[C, D]): IsEq[F[(A, C), (B, D)]] =
    (fab *** fcd) <-> fab.first.andThen(fcd.second)

  def arrowMergeConsistency[A, B, C](fab: F[A, B], fac: F[A, C]): IsEq[F[A, (B, C)]] =
    (fab &&& fac) <-> F.lift((x: A) => (x, x)).andThen(fab *** fac)
}

object ArrowLaws {
  def apply[F[_, _]: Arrow]: ArrowLaws[F] = new ArrowLaws[F] {
    val F: Arrow[F] = Arrow[F]
  }
}
