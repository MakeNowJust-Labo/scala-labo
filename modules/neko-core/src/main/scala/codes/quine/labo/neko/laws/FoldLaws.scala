package codes.quine.labo
package neko
package laws

import data._
import syntax._

trait FoldLaws[F[_]] {
  implicit val F: Fold[F]

  def foldRightLazy[A](fa: F[A]): IsEq[Int] = {
    var i = 0
    F.foldRight(fa, Eval.now("empty")) { (_, _) =>
        i += 1
        Eval.now("not empty")
      }
      .value
    i <-> (if (fa.isEmpty) 0 else 1)
  }

  def leftConststencyWithFoldMap[A, M: Monoid](fa: F[A], f: A => M): IsEq[M] =
    fa.foldMap(f) <-> fa.foldLeft(Monoid[M].empty)(_ |+| f(_))

  def rightConsistencyWithFoldMap[A, M: Monoid](fa: F[A], f: A => M): IsEq[M] =
    fa.foldMap(f) <-> fa.foldRight(Eval.now(Monoid[M].empty))((a, lb) => lb.map(f(a) |+| _)).value
}

object FoldLaws {
  def apply[F[_]: Fold]: FoldLaws[F] = new FoldLaws[F] {
    val F: Fold[F] = Fold[F]
  }
}
