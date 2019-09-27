package codes.quine.labo
package neko
package data

import scala.annotation.tailrec
import syntax._

sealed trait Eval[+A] {
  import Eval._

  def value: A

  def memoize: Eval[A]

  def map[B](f: A => B): Eval[B] = flatMap(a => Now(f(a)))

  def flatMap[B](f: A => Eval[B]): Eval[B] =
    this match {
      case FlatMap(fa, f1) => FlatMap(fa, f1.andThen(TATree.lift[EvalFun, A, B](f)))
      case _               => FlatMap(this, TATree.lift[EvalFun, A, B](f))
    }

  // It is only used for implementing `Semigroup[Eval[A]]` and `Monoid[Eval[A]]`.
  private[data] def concat[AA >: A](that: Eval[AA])(implicit AA: Semigroup[AA]): Eval[AA] =
    for { x <- this; y <- that } yield (x: AA) |+| y
}

object Eval extends EvalInstances0 {
  def now[A](a: A): Eval[A] = Now(a)
  def always[A](a: => A): Eval[A] = Always(a _)
  def later[A](a: => A): Eval[A] = Later(a _)
  def defer[A](fa: => Eval[A]): Eval[A] = Defer(fa _)

  final private case class Now[A](value: A) extends Eval[A] {
    def memoize: Eval[A] = this
  }

  final private case class Always[A](thunk: () => A) extends Eval[A] {
    def value: A = thunk()
    def memoize: Eval[A] = Later(thunk)
  }

  final private case class Later[A](var thunk: () => A) extends Eval[A] {
    lazy val value: A = {
      val a = thunk()
      thunk = null
      a
    }
    def memoize: Eval[A] = this
  }

  final private case class Memoize[A](var fa: Eval[A]) extends Eval[A] {
    var memo: Option[A] = None
    def value: A = evaluate(this)
    def memoize: Eval[A] = this
  }

  final private case class Defer[A](thunk: () => Eval[A]) extends Eval[A] {
    def value: A = evaluate(this)
    def memoize: Eval[A] = Memoize(this)
  }

  type EvalFun[A, B] = A => Eval[B]

  final private case class FlatMap[A, B](fa: Eval[A], f: TATree[EvalFun, A, B]) extends Eval[B] {
    def value: B = evaluate(this)
    def memoize: Eval[B] = Memoize(this)
  }

  private def evaluate[A](fa: Eval[A]): A = {
    import TATree._

    def memo[A1](m: Memoize[A1]): TATree[EvalFun, A1, A1] = TATree.lift[EvalFun, A1, A1] { a =>
      m.memo = Some(a)
      Now(a)
    }

    @tailrec def loop[A1, B1](fa: Eval[A1], f: TATree[EvalFun, A1, B1]): B1 =
      fa match {
        case FlatMap(fa1, f1) => loop(fa1, f1.andThen(f))
        case Defer(t)         => loop(t(), f)
        case m: Memoize[A1] =>
          m.memo match {
            case Some(a) =>
              f.view match {
                case _: View.Stop[EvalFun, A1] => a
                case View.Continue(f1, k)      => loop(f1(a), k)
              }
            case None => loop(m.fa, memo(m).andThen(f))
          }
        case _ =>
          f.view match {
            case _: View.Stop[EvalFun, A1] => fa.value
            case View.Continue(f1, k)      => loop(f1(fa.value), k)
          }
      }

    loop(fa, TATree.identity[EvalFun, A])
  }
}

private[data] trait EvalInstances0 extends EvalInstances1 {
  implicit val evalBimonadInstance: Bimonad[Eval] = new Bimonad[Eval] {
    def pure[A](a: A): Eval[A] = Eval.now(a)
    override def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)
    override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa.map(f)
    def tailRecM[A, B](a: A)(f: A => Eval[Either[A, B]]): Eval[B] =
      flatMap(f(a)) {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => pure(b)
      }
    def extract[A](fa: Eval[A]): A = fa.value
    def coflatMap[A, B](fa: Eval[A])(f: Eval[A] => B): Eval[B] = Eval.later(f(fa))
  }

  implicit val evalDeferInstance: Defer[Eval] = new Defer[Eval] {
    def defer[A](fa: => Eval[A]): Eval[A] = Eval.defer(fa)
  }

  implicit val evalFoldInstance: Fold[Eval] = new Fold[Eval] {
    def foldLeft[A, B](fa: Eval[A], b: B)(f: (B, A) => B): B = f(b, fa.value)
    def foldRight[A, B](fa: Eval[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.flatMap(a => f(a, lb))
  }

  implicit def evalEqInstance[A: Eq]: Eq[Eval[A]] = Eq[A].by(_.value)
  implicit def evalPartialOrdInstance[A: PartialOrd]: PartialOrd[Eval[A]] = PartialOrd[A].by(_.value)
  implicit def evalOrdInstance[A: Ord]: Ord[Eval[A]] = Ord[A].by(_.value)

  implicit def evalSemigroupInstance[A: Semigroup]: Semigroup[Eval[A]] = new Semigroup[Eval[A]] {
    def concat(x: Eval[A], y: Eval[A]): Eval[A] = x.concat(y)
  }

  implicit def evalMonoidInstance[A: Monoid]: Monoid[Eval[A]] = new Monoid[Eval[A]] {
    lazy val empty: Eval[A] = Eval.later(Monoid[A].empty)
    def concat(x: Eval[A], y: Eval[A]): Eval[A] = x.concat(y)
  }
}

private[data] trait EvalInstances1 {
  implicit def evalHashInstance[A: Hash]: Hash[Eval[A]] = new Hash[Eval[A]] {
    def eqv(x: Eval[A], y: Eval[A]): Boolean = x.value === y.value
    def hash(x: Eval[A]): Int = x.value.hash
  }
}
