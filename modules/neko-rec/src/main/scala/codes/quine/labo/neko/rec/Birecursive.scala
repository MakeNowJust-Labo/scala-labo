package codes.quine.labo
package neko
package rec

import neko.syntax._

trait Birecursive[F[_], T] extends Recursive[F, T] with Corecursive[F, T] {
  def lambek(t: T): F[T] = cata(t)((_: F[F[T]]).map(embed))

  def colambek(ft: F[T]): T = ana(ft)(_.map(project))
}

object Birecursive {
  def apply[F[_], T](implicit FT: Birecursive[F, T]): Birecursive[F, T] = FT
}
