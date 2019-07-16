package codes.quine.labo.nyan
package instances

trait Function1Instances {
  implicit object Function1Instances extends Arrow[Function1] {
    def id[A]: A => A = identity

    def compose[A, B, C](f: B => C)(g: A => B): A => C = f.compose(g)

    def lift[A, B](f: A => B): A => B = f

    def first[A, B, C](f: A => B): ((A, C)) => (B, C) = {
      case (a, c) => (f(a), c)
    }
  }
}
