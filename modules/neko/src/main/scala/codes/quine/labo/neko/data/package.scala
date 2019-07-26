package codes.quine.labo.neko

import syntax._

package object data {
  type ~>[F[_], G[_]] = FunctionK[F, G]

  type Reader[E, A] = ReaderT[E, Id, A]

  object Reader {
    def apply[E, A](run: E => A): ReaderT[E, Id, A] = ReaderT(e => Id(run(e)))

    def ask[E]: Reader[E, E] = ReaderT.ask[E, Id]
  }

  type State[S, A] = StateT[S, Id, A]

  object State {
    def apply[S, A](run: S => (S, A)): State[S, A] = StateT(s => Id(run(s)))

    def read[S]: State[S, S] = StateT.read[S, Id]

    def write[S](s: S): State[S, Unit] = StateT.write[S, Id](s)
  }

  type Kleisli[M[_], A, B] = ReaderT[A, M, B]

  object Kleisli {
    def apply[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = ReaderT(f)

    def lift[M[_], A, B](f: A => B)(implicit M: Monad[M]): Kleisli[M, A, B] = Kleisli(x => M.pure(f(x)))
  }

  implicit def KleisliInstances[M[_]](implicit M: Monad[M]): Arrow[Kleisli[M, ?, ?]] = new Arrow[Kleisli[M, ?, ?]] {
    def id[A]: Kleisli[M, A, A] = Kleisli(M.pure)

    def compose[A, B, C](f: Kleisli[M, B, C])(g: Kleisli[M, A, B]): Kleisli[M, A, C] = f.compose(g)

    override def andThen[A, B, C](f: Kleisli[M, A, B])(g: Kleisli[M, B, C]): Kleisli[M, A, C] = f.andThen(g)

    def lift[A, B](f: A => B): Kleisli[M, A, B] = Kleisli.lift(f)

    def first[A, B, C](f: Kleisli[M, A, B]): Kleisli[M, (A, C), (B, C)] = Kleisli {
      case (a, c) => f.run(a).map((_, c))
    }
  }
}
