package codes.quine.labo

import neko._, data._, syntax._, instances._

object trans {
  type M[A] = StateT[ReaderT[Writer[String, *], Int, *], Int, A]

  def addEnv[F[_]](implicit F: Monad[F], reader: MonadReader[F, Int], state: MonadState[F, Int]): F[Unit] =
    reader.ask.flatMap(e => state.modify(_ + e))

  def foo: M[Int] =
    for {
      _ <- addEnv[M]
      _ <- addEnv[M]
      s <- MonadReader.local((_: Int) => 20)(for {
        _ <- addEnv[M]
        s <- MonadState.get[M, Int]
      } yield s)
      _ <- MonadWriter.tell[M, String](s.toString)
    } yield s
}
