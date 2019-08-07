package codes.quine.labo
package neko
package laws

import syntax._

trait StrongLaws[F[_, _]] {
  implicit val F: Strong[F]

  def strongFirstSecondDimap[A, B, C](fab: F[A, B]): IsEq[F[(A, C), (B, C)]] =
    fab.first[C] <-> fab.second[C].dimap((_: (A, C)).swap)(_.swap)

  def strongSecondFirstDimap[A, B, C](fab: F[A, B]): IsEq[F[(C, A), (C, B)]] =
    fab.second[C] <-> fab.first[C].dimap((_: (C, A)).swap)(_.swap)

  def strongLmapFirstRmap[A, B, C](fab: F[A, B]): IsEq[F[(A, C), B]] =
    fab.lmap((_: (A, C))._1) <-> fab.first.rmap(_._1)

  def strongLmapSecondRmap[A, B, C](fab: F[A, B]): IsEq[F[(C, A), B]] =
    fab.lmap((_: (C, A))._2) <-> fab.second.rmap(_._2)

  private def mapFirst[A, B, C](f: A => B): ((A, C)) => (B, C) = { case (a, c)  => (f(a), c) }
  private def mapSecond[A, B, C](f: A => B): ((C, A)) => (C, B) = { case (c, a) => (c, f(a)) }

  def strongFirstDinaturality[A, B, C, D](fab: F[A, B], f: C => D): IsEq[F[(A, C), (B, D)]] =
    fab.first[C].rmap(mapSecond(f)) <-> fab.first[D].lmap(mapSecond(f))

  def strongSecondDinaturality[A, B, C, D](fab: F[A, B], f: C => D): IsEq[F[(C, A), (D, B)]] =
    fab.second[C].rmap(mapFirst(f)) <-> fab.second[D].lmap(mapFirst(f))

  private def assoc[A, B, C]: (((A, B), C)) => (A, (B, C)) = { case ((a, b), c)   => (a, (b, c)) }
  private def unassoc[A, B, C]: ((A, (B, C))) => ((A, B), C) = { case (a, (b, c)) => ((a, b), c) }

  def strongFirstFirstDimap[A, B, C, D](fab: F[A, B]): IsEq[F[((A, C), D), ((B, C), D)]] =
    fab.first[C].first[D] <-> fab.first[(C, D)].dimap[((A, C), D), ((B, C), D)](assoc)(unassoc)

  def strongSecondSecondDimap[A, B, C, D](fab: F[A, B]): IsEq[F[(D, (C, A)), (D, (C, B))]] =
    fab.second[C].second[D] <-> fab.second[(D, C)].dimap[(D, (C, A)), (D, (C, B))](unassoc)(assoc)
}

object StrongLaws {
  def apply[F[_, _]: Strong]: StrongLaws[F] = new StrongLaws[F] {
    val F: Strong[F] = Strong[F]
  }
}
