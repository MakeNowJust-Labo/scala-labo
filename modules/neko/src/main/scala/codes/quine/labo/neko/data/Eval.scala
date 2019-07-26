package codes.quine.labo.neko
package data

import scala.annotation.tailrec

sealed trait Eval[+A] { self =>
  import Eval._

  def value: A

  def memoize: Eval[A]

  def map[B](f: A => B): Eval[B] =
    flatMap(x => Now(f(x)))

  def flatMap[B](f: A => Eval[B]): Eval[B] =
    self match {
      case c: FlatMap[A] =>
        new FlatMap[B] {
          type S = c.S
          val start: () => Eval[c.S] = c.start
          val run: c.S => Eval[B] = s =>
            new FlatMap[B] {
              type S = A
              val start: () => Eval[A] = () => c.run(s)
              val run: A => Eval[B] = f
            }
        }
      case c: Defer[A] =>
        new FlatMap[B] {
          type S = A
          val start: () => Eval[A] = c.thunk
          val run: A => Eval[B] = f
        }
      case _ =>
        new FlatMap[B] {
          type S = A
          val start: () => Eval[A] = () => self
          val run: A => Eval[B] = f
        }
    }
}

object Eval {
  def now[A](a: A): Eval[A] = Now(a)

  def later[A](a: => A): Eval[A] = new Later(a _)

  def always[A](a: => A): Eval[A] = new Always(a _)

  def defer[A](fa: => Eval[A]): Eval[A] = new Defer(fa _) {}

  final private case class Now[A](value: A) extends Eval[A] { self =>
    def memoize: Eval[A] = self
  }

  final private class Always[A](f: () => A) extends Eval[A] {
    def value: A = f()

    def memoize: Eval[A] = new Later(f)
  }

  final private class Later[A](f: () => A) extends Eval[A] { self =>
    private[this] var thunk: () => A = f

    lazy val value: A = {
      val a = thunk()
      thunk = null
      a
    }

    def memoize: Eval[A] = self
  }

  abstract private class Defer[A](val thunk: () => Eval[A]) extends Eval[A] { self =>
    def memoize: Eval[A] = Memoize(self)
    def value: A = evaluate(self)
  }

  abstract private class FlatMap[A] extends Eval[A] { self =>
    type S
    val start: () => Eval[S]
    val run: S => Eval[A]

    def memoize: Eval[A] = Memoize(self)
    def value: A = evaluate(self)
  }

  private case class Memoize[A](eval: Eval[A]) extends Eval[A] { self =>
    var result: Option[A] = None
    def memoize: Eval[A] = self
    def value: A =
      result match {
        case Some(a) => a
        case None =>
          val a = evaluate(self)
          result = Some(a)
          a
      }
  }

  @tailrec private def advance[A](fa: Eval[A]): Eval[A] =
    fa match {
      case call: Defer[A] =>
        advance(call.thunk())
      case compute: FlatMap[A] =>
        new FlatMap[A] {
          type S = compute.S
          val start: () => Eval[S] = () => compute.start()
          val run: S => Eval[A] = s => advance1(compute.run(s))
        }
      case other => other
    }

  private def advance1[A](fa: Eval[A]): Eval[A] = advance(fa)

  def evaluate[A](eval: Eval[A]): A = {
    type L = Eval[Any]
    type M = Memoize[Any]
    type C = Any => Eval[Any]

    def addToMemo(m: M): C = { a: Any =>
      m.result = Some(a)
      Now(a)
    }

    @tailrec def loop(curr: L, fs: List[C]): Any =
      curr match {
        case c: FlatMap[_] =>
          c.start() match {
            case cc: FlatMap[_] =>
              loop(cc.start(), cc.run.asInstanceOf[C] :: c.run.asInstanceOf[C] :: fs)
            case mm @ Memoize(eval) =>
              mm.result match {
                case Some(a) =>
                  loop(Now(a), c.run.asInstanceOf[C] :: fs)
                case None =>
                  loop(eval, addToMemo(mm.asInstanceOf[M]) :: c.run.asInstanceOf[C] :: fs)
              }
            case xx => loop(c.run(xx.value), fs)
          }
        case call: Defer[_] =>
          loop(advance(call), fs)
        case m @ Memoize(eval) =>
          m.result match {
            case Some(a) =>
              fs match {
                case f :: fs => loop(f(a), fs)
                case Nil     => a
              }
            case None =>
              loop(eval, addToMemo(m) :: fs)
          }
        case x =>
          fs match {
            case f :: fs => loop(f(x.value), fs)
            case Nil     => x.value
          }
      }

    loop(eval.asInstanceOf[L], Nil).asInstanceOf[A]
  }

  implicit object EvalInstances extends Monad[Eval] {
    def pure[A](a: A): Eval[A] = Eval.now(a)

    override def map[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa.map(f)

    def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] = fa.flatMap(f)
  }

  implicit def EvalEqInstances[A: Eq]: Eq[Eval[A]] = Eq[A].by(_.value)
}
