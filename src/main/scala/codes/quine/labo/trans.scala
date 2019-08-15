package codes.quine.labo

import neko._, data._, syntax._

object trans {
  type M[A] = StateT[Reader[Int, *], Int, A]
  implicit val mMonadReaderInstance = MonadReader.trans[StateT[*[_], Int, *], Reader[Int, *], Int]

  def addEnv[F[_]](implicit F: Monad[F], reader: MonadReader[F, Int], state: MonadState[F, Int]): F[Unit] =
    reader.ask.flatMap(e => state.modify(_ + e))

  def foo: M[Unit] =
    for {
      _ <- addEnv[M]
      _ <- addEnv[M]
    } yield ()
}
