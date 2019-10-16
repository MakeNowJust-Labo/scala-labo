package codes.quine.labo
package hollow

trait Concat[T, U] {
  type Out
  def concat(t: T, u: U): Out
}

object Concat extends ConcatInstances0 {
  type Aux[T, U, R] = Concat[T, U] { type Out = R }
  def apply[T, U](implicit TU: Concat[T, U]): Aux[T, U, TU.Out] = TU
}

private[hollow] trait ConcatInstances0 extends ConcatInstances1 {
  implicit val charChar: Concat.Aux[Char, Char, String] = new Concat[Char, Char] {
    type Out = String
    def concat(t: Char, u: Char): String = s"${t}${u}"
  }

  implicit val stringChar: Concat.Aux[String, Char, String] = new Concat[String, Char] {
    type Out = String
    def concat(t: String, u: Char): String = s"${t}${u}"
  }

  implicit val charString: Concat.Aux[Char, String, String] = new Concat[Char, String] {
    type Out = String
    def concat(t: Char, u: String): String = s"${t}${u}"
  }

  implicit val stringString: Concat.Aux[String, String, String] = new Concat[String, String] {
    type Out = String
    def concat(t: String, u: String): String = s"${t}${u}"
  }

  implicit def leftUnit[A]: Concat.Aux[A, Unit, A] = new Concat[A, Unit] {
    type Out = A
    def concat(t: A, u: Unit): A = t
  }
}

private[hollow] trait ConcatInstances1 extends ConcatInstances2 {
  implicit def rightUnit[A]: Concat.Aux[Unit, A, A] = new Concat[Unit, A] {
    type Out = A
    def concat(t: Unit, u: A): A = u
  }
}

private[hollow] trait ConcatInstances2 {
  implicit def generic[A, B]: Concat.Aux[A, B, A ~ B] = new Concat[A, B] {
    type Out = A ~ B
    def concat(t: A, u: B): (A, B) = (t, u)
  }
}
