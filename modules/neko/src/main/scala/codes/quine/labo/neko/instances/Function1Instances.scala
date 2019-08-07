package codes.quine.labo
package neko
package instances

trait Function1Instances extends Function1Instances0

trait Function1Instances0 {
  implicit val function1ArrowInstance: Arrow[Function1] = new Arrow[Function1] {
    def identity[A]: A => A = x => x
    def compose[A, B, C](f: B => C, g: A => B): A => C = f.compose(g)
    override def andThen[A, B, C](f: A => B, g: B => C): A => C = f.andThen(g)

    def lift[A, B](f: A => B): A => B = f

    def first[A, B, C](f: A => B): ((A, C)) => (B, C) = { case (a, c)           => (f(a), c) }
    override def second[A, B, C](f: A => B): ((C, A)) => (C, B) = { case (c, a) => (c, f(a)) }
  }
}

package object function1 extends Function1Instances
