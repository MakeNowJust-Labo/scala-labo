package codes.quine.labo
package neko
package data

import scala.annotation.tailrec

sealed trait Eval[A] {
  import Eval._

  def value: A

  def memoize: Eval[A]

  def map[B](f: A => B): Eval[B] = flatMap(a => Now(f(a)))

  def flatMap[B](f: A => Eval[B]): Eval[B] =
    this match {
      case FlatMap(fa, f1) => FlatMap(fa, f1 :+ f)
      case _               => FlatMap(this, FunTree.lift(f))
    }
}

object Eval {
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
  private sealed trait FunTree[A, B] {
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
    /** A single A => Eval[B] function.  */
    final case class Leaf[A, B](f: A => Eval[B]) extends FunTree[A, B]
    /** A => Eval[B] and B => Eval[C] functions composition. */
    final case class Node[A, B, C](l: FunTree[A, B], r: FunTree[B, C]) extends FunTree[A, C]

    def lift[A, B](f: A => Eval[B]): FunTree[A, B] = Leaf(f)
    def empty[A]: FunTree[A, A] = Empty()

    def concat[A, B, C](l: FunTree[A, B], r: FunTree[B, C]): FunTree[A, C] =
      (l, r) match {
        case (_: Empty[A], _) => r
        case (_, _: Empty[B]) => l
        case _                 => Node(l, r)
      }

    def append[A, B, C](l: FunTree[A, B], f: B => Eval[C]): FunTree[A, C] = concat(l, Leaf(f))
  }

  /** [[FunTree]] view. */
  private sealed trait FunList[A, B]

  private object FunList {
    import FunTree._

    final case class FLNil[A]() extends FunList[A, A]
    final case class FLCons[A, B, C](f: A => Eval[B], k: FunTree[B, C]) extends FunList[A, C]

    def from[A, B](f: FunTree[A, B]): FunList[A, B] =
      f match {
        case _: Empty[A]   => FLNil()
        case l: Leaf[A, B] => FLCons(l.f, FunTree.empty)
        case Node(l, r) =>
          @tailrec def loop[A1, B1, C1](l: FunTree[A1, B1], r: FunTree[B1, C1]): FunList[A1, C1] =
            l match {
              case _: Empty[A1]    => loop(r, FunTree.empty)
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
        case Defer(t)       => loop(t(), f)
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

  implicit def EvalEqInstances[A: Eq]: Eq[Eval[A]] = Eq[A].by(_.value)

  implicit object EvalInstances extends Monad[Eval] with neko.Defer[Eval] {
    def pure[A](a: A): Eval[A] = Now(a)
    def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)
    override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa.map(f)

    def defer[A](fa: => Eval[A]): Eval[A] = Defer(() => fa)
  }
}
