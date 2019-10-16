package codes.quine.labo
package hollow

trait Optional[T] {
  type Out
  def optional(o: Option[T]): Out
}

object Optional extends OptionalInstances0 {
  type Aux[T, R] = Optional[T] { type Out = R }
  def apply[T](implicit T: Optional[T]): Aux[T, T.Out] = T
}

private[hollow] trait OptionalInstances0 {
  implicit val char: Optional.Aux[Char, String] = new Optional[Char] {
    type Out = String
    def optional(o: Option[Char]): String = o.fold("")(_.toString)
  }

  implicit val string: Optional.Aux[String, String] = new Optional[String] {
    type Out = String
    def optional(o: Option[String]): String = o.getOrElse("")
  }

  implicit val unit: Optional.Aux[Unit, Unit] = new Optional[Unit] {
    type Out = Unit
    def optional(o: Option[Unit]): Unit = ()
  }
}

private[hollow] trait OptionalInstances1 {
  implicit def generic[A]: Optional.Aux[A, Option[A]] = new Optional[A] {
    type Out = Option[A]
    def optional(o: Option[A]): Option[A] = o
  }
}
