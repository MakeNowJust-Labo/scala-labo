package codes.quine.labo

final class Lazy[A] private (private[this] var thunk: () => A) {
  lazy val value = {
    val a = thunk()
    thunk = null
    a
  }

  override def toString: String =
    if (thunk == null) value.toString else "<not computed>"

  def isEvaluated: Boolean = thunk == null
}

object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy(value _)
}
