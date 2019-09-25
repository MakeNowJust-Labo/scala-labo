package codes.quine.labo
package neko
package data

import instances.string._, syntax._

final case class Const[A, B](value: A) {
  def retag[C]: Const[A, C] = this.asInstanceOf[Const[A, C]]
  def concat[C, D](that: Const[A, C])(implicit A: Semigroup[A]): Const[A, D] = Const(value |+| that.value)
}

object Const extends ConstInstances0 {
  def empty[A: Monoid, B]: Const[A, B] = Const(Monoid[A].empty)
}

private[data] trait ConstInstances0 extends ConstInstances1 {
  implicit def constFunctorInstance[A]: Functor[Const[A, *]] = new Functor[Const[A, *]] {
    def map[B, C](fb: Const[A, B])(f: B => C): Const[A, C] = fb.retag
  }

  implicit def constContravariantInstance[A]: Contravariant[Const[A, *]] = new Contravariant[Const[A, *]] {
    def contramap[B, C](fb: Const[A, B])(f: C => B): Const[A, C] = fb.retag
  }

  implicit def constApplicativeInstance[A: Monoid]: Applicative[Const[A, *]] = new Applicative[Const[A, *]] {
    def pure[B](b: B): Const[A, B] = Const.empty
    def ap[B, C](ff: Const[A, B => C])(fb: Const[A, B]): Const[A, C] = ff.concat(fb)
  }

  implicit def constEqInstance[A: Eq, B]: Eq[Const[A, B]] = Eq[A].by(_.value)
  implicit def constPartialOrdInstance[A: PartialOrd, B]: PartialOrd[Const[A, B]] = PartialOrd[A].by(_.value)
  implicit def constOrdInstance[A: Ord, B]: Ord[Const[A, B]] = Ord[A].by(_.value)

  implicit def constSemigroupInstance[A: Semigroup, B]: Semigroup[Const[A, B]] = constSemigroupKInstance[A].algebra[B]
  implicit def constMonoidInstance[A: Monoid, B]: Monoid[Const[A, B]] = constMonoidKInstance[A].algebra[B]

  implicit def constSemigroupKInstance[A: Semigroup]: SemigroupK[Const[A, *]] = new SemigroupK[Const[A, *]] {
    def concatK[B](x: Const[A, B], y: Const[A, B]): Const[A, B] = x.concat(y)
  }

  implicit def constMonoidKInstance[A: Monoid]: MonoidK[Const[A, *]] = new MonoidK[Const[A, *]] {
    def emptyK[B]: Const[A, B] = Const.empty[A, B]
    def concatK[B](x: Const[A, B], y: Const[A, B]): Const[A, B] = x.concat(y)
  }
}

private[data] trait ConstInstances1 {
  implicit def constHashInstance[A: Hash, B]: Hash[Const[A, B]] = new Hash[Const[A, B]] {
    def eqv(x: Const[A, B], y: Const[A, B]): Boolean = x.value === y.value
    def hash(x: Const[A, B]): Int = "Const".hash * 31 + x.value.hash
  }
}
