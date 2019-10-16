package codes.quine.labo
package neko
package data

final case class Endo[A](f: AndThen[A, A]) {
  def apply(a: A): A = f(a)
  def andThen(that: Endo[A]): Endo[A] = Endo(f.andThen(that.f))
  def compose(that: Endo[A]): Endo[A] = Endo(f.compose(that.f))
}

object Endo extends EndoInstances {
  def lift[A](f: A => A): Endo[A] = Endo(AndThen.lift(f))
  def empty[A]: Endo[A] = Endo(AndThen.identity[A])
}

private[data] trait EndoInstances {
  implicit def endoMonoidInstance[A]: MonoidK[Endo] = new MonoidK[Endo] {
    def emptyK[A]: Endo[A] = Endo.empty
    def concatK[A](x: Endo[A], y: Endo[A]): Endo[A] = x.compose(y)
  }
}
