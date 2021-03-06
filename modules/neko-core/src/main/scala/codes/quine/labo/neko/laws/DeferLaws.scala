package codes.quine.labo
package neko
package laws

trait DeferLaws[F[_]] {
  implicit val F: Defer[F]

  def deferIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> F.defer(fa)

  def deferDoesNotEvaluate[A](fa: Unit => F[A]): IsEq[Boolean] = {
    var evaluated = false
    var _ = F.defer {
      evaluated = true
      fa(())
    }
    evaluated <-> false
  }

  def deferStackSafety[A](fa: Unit => F[A]): IsEq[F[A]] = {
    def loop(c: Int): F[A] =
      if (c <= 0) F.defer(fa(()))
      else F.defer(loop(c - 1))

    loop(50000) <-> fa(())
  }
}

object DeferLaws {
  def apply[F[_]: Defer]: DeferLaws[F] = new DeferLaws[F] {
    val F = Defer[F]
  }
}
