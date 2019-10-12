package codes.quine.labo

import neko.{Applicative, Functor, Traverse}, neko.data.Eval, neko.syntax._
import neko.rec.Recursive, neko.rec.data.{Mu, Nu}

sealed trait ListF[T, A] extends Serializable with Product
final case class NilF[T, A]() extends ListF[T, A]
final case class ConsF[T, A](head: T, tail: A) extends ListF[T, A]

object ListF {
  implicit def listFTraverseInstance[T]: Traverse[ListF[T, *]] = new Traverse[ListF[T, *]] {
    def traverse[G[_]: Applicative, A, B](fa: ListF[T, A])(f: A => G[B]): G[ListF[T, B]] = fa match {
      case NilF()      => Applicative[G].pure(NilF())
      case ConsF(t, a) => f(a).map(ConsF(t, _))
    }

    def foldLeft[A, B](fa: ListF[T, A], b: B)(f: (B, A) => B): B = fa match {
      case NilF()      => b
      case ConsF(t, a) => f(b, a)
    }
    def foldRight[A, B](fa: ListF[T, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case NilF()      => lb
      case ConsF(t, a) => Eval.defer(f(a, lb))
    }
  }

  type MuList[T] = Mu[ListF[T, *]]

  object MuList {
    def empty[T]: MuList[T] = Mu(NilF())
    def cons[T](h: T, t: MuList[T]): MuList[T] = Mu(ConsF(h, t))
  }

  implicit class MuListOps[T](val mu: MuList[T]) extends AnyVal {
    def toList: List[T] = Recursive[ListF[T, *], MuList[T]].cata[List[T]](mu) {
      case NilF()      => List.empty
      case ConsF(t, a) => t :: a
    }
  }

  type LazyListF[T, A] = ListF[Eval[T], Eval[A]]

  implicit def lazyListFFunctorInstance[T]: Functor[LazyListF[T, *]] = new Functor[LazyListF[T, *]] {
    def map[A, B](fa: LazyListF[T, A])(f: A => B): LazyListF[T, B] = fa match {
      case NilF()      => NilF()
      case ConsF(t, a) => ConsF(t, a.map(f))
    }
  }

  type NuList[T] = Nu[LazyListF[T, *]]

  object NuList {
    def cons[T](h: => T, t: => NuList[T]): NuList[T] =
      Nu[LazyListF[T, *], Either[Eval[T], NuList[T]]](Left(Eval.later(h))) {
        case Left(h)  => ConsF(h, Eval.later(Right(t)))
        case Right(t) => t.unNu(t.a).map(_.map(ta => Right(Nu[LazyListF[T, *], t.A](ta)(t.unNu(_)))))
      }

    def iterate[T](t: T)(f: T => T): NuList[T] =
      Nu[LazyListF[T, *], T](t)(t => ConsF(Eval.now(t), Eval.now(f(t))))
  }

  implicit class NuListOps[T](val nu: NuList[T]) extends AnyVal {
    def toLazyList: LazyList[T] = Recursive[LazyListF[T, *], NuList[T]].cata[LazyList[T]](nu) {
      case NilF()      => LazyList.empty
      case ConsF(t, a) => LazyList.cons(t.value, a.value)
    }
  }
}
