package codes.quine.labo.nyan.data

trait FunctionK[F[_], G[_]] { self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](t: FunctionK[E, F]): FunctionK[E, G] =
    new FunctionK[E, G] {
      def apply[A](ea: E[A]): G[A] = self(t(ea))
    }

  def andThen[H[_]](t: FunctionK[G, H]): FunctionK[F, H] =
    new FunctionK[F, H] {
      def apply[A](fa: F[A]): H[A] = t(self(fa))
    }
}
