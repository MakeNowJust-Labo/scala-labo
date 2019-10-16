package codes.quine.labo
package hollow

trait Repeat[T] {
  type Out
  def repeat(ts: List[T]): Out
}

object Repeat extends RepeatInstances0 {
  type Aux[T, R] = Repeat[T] { type Out = R }
  def apply[T](implicit T: Repeat[T]): Aux[T, T.Out] = T
}

private[hollow] trait RepeatInstances0 {
  implicit val char: Repeat.Aux[Char, String] = new Repeat[Char] {
    type Out = String
    def repeat(ts: List[Char]): String = ts.mkString
  }

  implicit val string: Repeat.Aux[String, String] = new Repeat[String] {
    type Out = String
    def repeat(ts: List[String]): String = ts.mkString
  }

  implicit val unit: Repeat.Aux[Unit, Unit] = new Repeat[Unit] {
    type Out = Unit
    def repeat(ts: List[Unit]): Unit = ()
  }
}

private[hollow] trait RepeatInstances1 {
  implicit def generic[A]: Repeat.Aux[A, List[A]] = new Repeat[A] {
    type Out = List[A]
    def repeat(ts: List[A]): List[A] = ts
  }
}
