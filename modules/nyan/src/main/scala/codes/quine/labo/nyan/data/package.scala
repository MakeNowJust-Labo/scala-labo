package codes.quine.labo.nyan

package object data {
  type ~>[F[_], G[_]] = FunctionK[F, G]

  type Reader[E, A] = ReaderT[E, Id, A]

  object Reader {
    def apply[E, A](run: E => A): ReaderT[E, Id, A] = ReaderT(e => Id(run(e)))

    def read[E]: Reader[E, E] = ReaderT.read[E, Id]
  }

  type State[S, A] = StateT[S, Id, A]

  object State {
    def apply[S, A](run: S => (S, A)): State[S, A] = StateT(s => Id(run(s)))

    def read[S]: State[S, S] = StateT.read[S, Id]

    def write[S](s: S): State[S, Unit] = StateT.write[S, Id](s)
  }
}
