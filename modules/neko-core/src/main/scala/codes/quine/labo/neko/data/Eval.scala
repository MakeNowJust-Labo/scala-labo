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
      case FlatMap(fa, f1) => FlatMap(fa, f1 :+ f)
      case _               => FlatMap(this, FunTree.lift(f))
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

  /**
   * A type-safe & fast A => Eval[B] function composer.
   *
   * It represents function compositions as binary tree.
   *
   * @see [[http://hackage.haskell.org/package/type-aligned]]
   */
  sealed private trait FunTree[-A, +B] {

    /** Append a function to the last. O(1) */
    def :+[C](f: B => Eval[C]): FunTree[A, C] = FunTree.append(this, f)

    /** Concat a function list. O(1) */
    def ++[C](r: FunTree[B, C]): FunTree[A, C] = FunTree.concat(this, r)

    /** Extract the head and rest functions. Amortized O(1) */
    def view: FunList[A, B] = FunList.from(this)
  }

  private object FunTree {

    /** An empty function composition (Eval.now: A => Eval[A]). */
    final case class Empty[A]() extends FunTree[A, A]

    object Empty {
      private[this] val instance: Empty[Any] = new Empty[Any]
      def apply[A]: Empty[A] = instance.asInstanceOf[Empty[A]]
    }

    /** A single A => Eval[B] function.  */
    final case class Leaf[-A, +B](f: A => Eval[B]) extends FunTree[A, B]

    /** A => Eval[B] and B => Eval[C] functions composition. */
    final case class Node[-A, B, +C](l: FunTree[A, B], r: FunTree[B, C]) extends FunTree[A, C]

    def lift[A, B](f: A => Eval[B]): FunTree[A, B] = Leaf(f)
    def empty[A]: FunTree[A, A] = Empty[A]

    def concat[A, B, C](l: FunTree[A, B], r: FunTree[B, C]): FunTree[A, C] =
      (l, r) match {
        case (_: Empty[A], _) => r
        case (_, _: Empty[B]) => l
        case _                => Node(l, r)
      }

    def append[A, B, C](l: FunTree[A, B], f: B => Eval[C]): FunTree[A, C] = concat(l, Leaf(f))
  }

  /** [[FunTree]] view. */
  sealed private trait FunList[-A, +B]

  private object FunList {
    import FunTree._

    final case class FLNil[A]() extends FunList[A, A]

    object FLNil {
      private[this] val instance: FLNil[Any] = new FLNil[Any]
      def apply[A]: FLNil[A] = instance.asInstanceOf[FLNil[A]]
    }

    final case class FLCons[-A, B, +C](f: A => Eval[B], k: FunTree[B, C]) extends FunList[A, C]

    def from[A, B](f: FunTree[A, B]): FunList[A, B] =
      f match {
        case _: Empty[A]   => FLNil[A]
        case l: Leaf[A, B] => FLCons(l.f, FunTree.empty[B])
        case Node(l, r) =>
          @tailrec def loop[A1, B1, C1](l: FunTree[A1, B1], r: FunTree[B1, C1]): FunList[A1, C1] =
            l match {
              case _: Empty[A1]    => loop(r.asInstanceOf[FunTree[A1, C1]], FunTree.empty[C1])
              case l: Leaf[A1, B1] => FLCons(l.f, r)
              case Node(l1, r1)    => loop(l1, r1 ++ r)
            }
          loop(l, r)
      }
  }

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

  final private case class FlatMap[A, B](fa: Eval[A], f: FunTree[A, B]) extends Eval[B] {
    def value: B = evaluate(this)
    def memoize: Eval[B] = Memoize(this)
  }

  private def evaluate[A](fa: Eval[A]): A = {
    import FunList._

    def memo[A1](m: Memoize[A1]): FunTree[A1, A1] = FunTree.lift { a =>
      m.memo = Some(a)
      Now(a)
    }

    @tailrec def loop[A1, B1](fa: Eval[A1], f: FunTree[A1, B1]): B1 =
      fa match {
        case FlatMap(fa1, f1) => loop(fa1, f1 ++ f)
        case Defer(t)         => loop(t(), f)
        case m: Memoize[A1] =>
          m.memo match {
            case Some(a) =>
              f.view match {
                case _: FLNil[A1]  => a
                case FLCons(f1, k) => loop(f1(a), k)
              }
            case None => loop(m.fa, memo(m) ++ f)
          }
        case _ =>
          f.view match {
            case _: FLNil[A1]  => fa.value
            case FLCons(f1, k) => loop(f1(fa.value), k)
          }
      }

    loop(fa, FunTree.empty[A])
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
