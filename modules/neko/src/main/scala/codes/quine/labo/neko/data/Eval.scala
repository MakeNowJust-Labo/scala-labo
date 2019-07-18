package codes.quine.labo
package neko
package data

import scala.annotation.tailrec

trait Eval[A] {
  import Eval._

  def value: A

  def memoize: Eval[A]

  def map[B](f: A => B): Eval[B] = flatMap(a => Now(f(a)))

  def flatMap[B](f: A => Eval[B]): Eval[B] =
    this match {
      case FlatMap(fa, f1) => FlatMap(fa, f1 :+ f)
      case _               => FlatMap(this, AndThen.lift(f))
    }
}

object Eval {
  def now[A](a: A): Eval[A] = Now(a)
  def always[A](a: => A): Eval[A] = Always(a _)
  def later[A](a: => A): Eval[A] = Later(a _)
  def defer[A](fa: => Eval[A]): Eval[A] = Defer(fa _)

  /**
   * A type-safe & fast function composer.
   *
   * @see [[http://hackage.haskell.org/package/type-aligned]]
   */
  private trait AndThen[A, B] {

    /** Append a function to the last. O(1) */
    def :+[C](f: B => Eval[C]): AndThen[A, C] = AndThen.append(this, f)

    /** Concat a function list. O(1) */
    def ++[C](r: AndThen[B, C]): AndThen[A, C] = AndThen.concat(this, r)

    /** Extract the head and rest functions. Amortized O(1) */
    def view: AndThenView[A, B] = AndThenView.from(this)
  }

  private object AndThen {
    def lift[A, B](f: A => Eval[B]): AndThen[A, B] = Leaf(f)
    def empty[A]: AndThen[A, A] = Empty()

    def concat[A, B, C](l: AndThen[A, B], r: AndThen[B, C]): AndThen[A, C] =
      (l, r) match {
        case (_: Empty[A], r1) => r
        case (l1, _: Empty[B]) => l
        case _                 => Node(l, r)
      }

    def append[A, B, C](l: AndThen[A, B], f: B => Eval[C]): AndThen[A, C] = concat(l, Leaf(f))

    final case class Empty[A]() extends AndThen[A, A]
    final case class Leaf[A, B](f: A => Eval[B]) extends AndThen[A, B]
    final case class Node[A, B, C](l: AndThen[A, B], r: AndThen[B, C]) extends AndThen[A, C]
  }

  private trait AndThenView[A, B]

  private object AndThenView {
    import AndThen._

    final case class VNil[A]() extends AndThenView[A, A]
    final case class VCons[A, B, C](f: A => Eval[B], k: AndThen[B, C]) extends AndThenView[A, C]

    def from[A, B](f: AndThen[A, B]): AndThenView[A, B] =
      f match {
        case _: Empty[A]   => VNil()
        case l: Leaf[A, B] => VCons(l.f, AndThen.empty)
        case Node(l, r) =>
          @tailrec def loop[A1, B1, C1](l: AndThen[A1, B1], r: AndThen[B1, C1]): AndThenView[A1, C1] =
            l match {
              case _: Empty[A1]    => loop(r, AndThen.empty)
              case l: Leaf[A1, B1] => VCons(l.f, r)
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

  final private case class FlatMap[A, B](fa: Eval[A], f: AndThen[A, B]) extends Eval[B] {
    def value: B = evaluate(this)
    def memoize: Eval[B] = Memoize(this)
  }

  private def evaluate[A](fa: Eval[A]): A = {
    import AndThenView._

    def memo[A1](m: Memoize[A1]): AndThen[A1, A1] = AndThen.lift { a =>
      m.memo = Some(a)
      Now(a)
    }

    @tailrec def loop[A1, B1](fa: Eval[A1], f: AndThen[A1, B1]): B1 =
      fa match {
        case FlatMap(fa1, f1) => loop(fa1, f1 ++ f)
        case Defer(t)       => loop(t(), f)
        case m: Memoize[A1] =>
          m.memo match {
            case Some(a) =>
              f.view match {
                case _: VNil[A1]  => a
                case VCons(f1, k) => loop(f1(a), k)
              }
            case None => loop(m.fa, memo(m) ++ f)
          }
        case _ =>
          f.view match {
            case _: VNil[A1]  => fa.value
            case VCons(f1, k) => loop(f1(fa.value), k)
          }
      }

    loop(fa, AndThen.empty[A])
  }

  implicit def EvalEqInstances[A: Eq]: Eq[Eval[A]] = Eq[A].by(_.value)

  implicit object EvalInstances extends Monad[Eval] with neko.Defer[Eval] {
    def pure[A](a: A): Eval[A] = Now(a)
    def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)
    override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa.map(f)

    def defer[A](fa: => Eval[A]): Eval[A] = Defer(() => fa)
  }
}
