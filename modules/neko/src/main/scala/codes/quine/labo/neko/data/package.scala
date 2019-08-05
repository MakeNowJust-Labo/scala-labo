package codes.quine.labo
package neko

package object data {
  type ~>[F[_], G[_]] = FunctionK[F, G]

  type Cont[R, A] = ContT[Eval, R, A]

  object Cont {
    def now[R, A](a: A): Cont[R, A] = ContT.now(a)
    def always[R, A](a: => A): Cont[R, A] = ContT.always(a)
    def defer[R, A](fa: => Cont[R, A]): Cont[R, A] = ContT.defer(fa)

    def callCC[R, A](run: (A => Eval[R]) => Eval[R]): Cont[R, A] = ContT.callCC(run)
  }

  type Reader[E, A] = ReaderT[Id, E, A]

  object Reader {
    def run[E, A](fa: Reader[E, A])(e: E): A = ReaderT.run(fa)(e).value
    def apply[E, A](run: E => A): ReaderT[Id, E, A] = ReaderT(e => Id(run(e)))

    def ask[E]: Reader[E, E] = ReaderT.ask[Id, E]
    def local[E, A](f: E => E)(fa: Reader[E, A]): Reader[E, A] = ReaderT.local(f)(fa)
  }

  type State[S, A] = StateT[Id, S, A]

  object State {
    def run[S, A](fa: State[S, A])(s: S): (S, A) = StateT.run(fa)(s).value
    def exec[S, A](fa: State[S, A])(s: S): S = run(fa)(s)._1
    def eval[S, A](fa: State[S, A])(s: S): A = run(fa)(s)._2
    def apply[S, A](run: S => (S, A)): State[S, A] = StateT(s => Id(run(s)))

    def get[S]: State[S, S] = StateT.get[Id, S]
    def put[S](s: S): State[S, Unit] = StateT.put(s)
    def modify[S](f: S => S): State[S, Unit] = StateT.modify(f)
  }
}
