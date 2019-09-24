package codes.quine.labo

object logic {
  type Not[A] = A => Nothing
  type And[A, B] = A with B
  type Or[A, B] = Not[Not[A] And Not[B]]

  def x[A](x: A)(implicit ev: Not[Not[A]] <:< Or[Int, String]): String =
    x match {
      case i: Int    => s"$i: Int"
      case s: String => s"$s: String"
    }
}
