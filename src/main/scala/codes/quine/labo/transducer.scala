package codes.quine.labo
package transducer

import scala.collection.mutable
import simulacrum.typeclass

object `package` extends Reducing.ToReducingOps

trait Reducer[A, B] {
  type State

  def init: State
  def step(s: State, a: A): State
  def complete(s: State): B
}

object Reducer {
  def list[A]: Reducer[A, List[A]] = new Reducer[A, List[A]] {
    type State = mutable.Builder[A, List[A]]

    def init: mutable.Builder[A, List[A]] = List.newBuilder
    def step(s: mutable.Builder[A, List[A]], a: A): mutable.Builder[A, List[A]] =
      s += a
    def complete(s: mutable.Builder[A, List[A]]): List[A] =
      s.result
  }

  def vector[A]: Reducer[A, Vector[A]] = new Reducer[A, Vector[A]] {
    type State = mutable.Builder[A, Vector[A]]

    def init: mutable.Builder[A, Vector[A]] = Vector.newBuilder
    def step(s: mutable.Builder[A, Vector[A]], a: A): mutable.Builder[A, Vector[A]] =
      s += a
    def complete(s: mutable.Builder[A, Vector[A]]): Vector[A] = s.result
  }

  def first[A]: Reducer[A, Option[A]] = new Reducer[A, Option[A]] {
    type State = Option[A]

    def init: Option[A] = None
    def step(s: Option[A], a: A): Option[A] =
      s match {
        case None       => Some(a)
        case r: Some[A] => r
      }
    def complete(s: Option[A]): Option[A] = s
  }

  def last[A]: Reducer[A, Option[A]] = new Reducer[A, Option[A]] {
    type State = Option[A]

    def init: Option[A] = None
    def step(s: Option[A], a: A): Option[A] =
      Some(a)
    def complete(s: Option[A]): Option[A] = s
  }

  def sum[A](implicit A: math.Numeric[A]): Reducer[A, A] = new Reducer[A, A] {
    type State = A

    def init: A = A.zero
    def step(s: A, a: A): A = A.plus(s, a)
    def complete(s: A): A = s
  }
}

trait Transducer[A, B] { xf =>
  def trans[S](rf: Reducer[B, S]): Reducer[A, S]

  def >->[C](xg: Transducer[B, C]): Transducer[A, C] =
    new Transducer[A, C] {
      def trans[S](rf: Reducer[C, S]): Reducer[A, S] = xf.trans(xg.trans(rf))
    }
}

object Transducer {
  def map[A, B](f: A => B): Transducer[A, B] = new Transducer[A, B] {
    def trans[C](rf: Reducer[B, C]): Reducer[A, C] = new Reducer[A, C] {
      type State = rf.State

      def init: rf.State = rf.init
      def step(s: rf.State, a: A): rf.State = rf.step(s, f(a))
      def complete(s: rf.State): C = rf.complete(s)
    }
  }

  def filter[A](f: A => Boolean): Transducer[A, A] = new Transducer[A, A] {
    def trans[B](rf: Reducer[A, B]): Reducer[A, B] = new Reducer[A, B] {
      type State = rf.State

      def init: rf.State = rf.init
      def step(s: rf.State, a: A): rf.State = if (f(a)) rf.step(s, a) else s
      def complete(s: rf.State): B = rf.complete(s)
    }
  }

  def take[A](n: Int): Transducer[A, A] = new Transducer[A, A] {
    def trans[B](rf: Reducer[A, B]): Reducer[A, B] = new Reducer[A, B] {
      type State = (Int, rf.State)

      def init: (Int, rf.State) = (0, rf.init)
      def step(s: (Int, rf.State), a: A): (Int, rf.State) =
        if (s._1 < n) (s._1 + 1, rf.step(s._2, a)) else s
      def complete(s: (Int, rf.State)): B = rf.complete(s._2)
    }
  }

  def drop[A](n: Int): Transducer[A, A] = new Transducer[A, A] {
    def trans[B](rf: Reducer[A, B]): Reducer[A, B] = new Reducer[A, B] {
      type State = (Int, rf.State)

      def init: (Int, rf.State) = (0, rf.init)
      def step(s: (Int, rf.State), a: A): (Int, rf.State) =
        if (s._1 < n) (s._1 + 1, s._2) else (s._1, rf.step(s._2, a))
      def complete(s: (Int, rf.State)): B = rf.complete(s._2)
    }
  }

  def withIndex[A]: Transducer[A, (A, Int)] = new Transducer[A, (A, Int)] {
    def trans[B](rf: Reducer[(A, Int), B]): Reducer[A, B] = new Reducer[A, B] {
      type State = (Int, rf.State)

      def init: (Int, rf.State) = (0, rf.init)
      def step(s: (Int, rf.State), a: A): (Int, rf.State) =
        (s._1 + 1, rf.step(s._2, (a, s._1)))
      def complete(s: (Int, rf.State)): B = rf.complete(s._2)
    }
  }

  def dedupe[A]: Transducer[A, A] = new Transducer[A, A] {
    def trans[B](rf: Reducer[A, B]): Reducer[A, B] = new Reducer[A, B] {
      type State = (Option[A], rf.State)

      def init: (Option[A], rf.State) = (None, rf.init)
      def step(s: (Option[A], rf.State), a: A): (Option[A], rf.State) =
        (Some(a), if (s._1.exists(_ == a)) s._2 else rf.step(s._2, a))
      def complete(s: (Option[A], rf.State)): B = rf.complete(s._2)
    }
  }

  def distinct[A]: Transducer[A, A] = new Transducer[A, A] {
    def trans[B](rf: Reducer[A, B]): Reducer[A, B] = new Reducer[A, B] {
      type State = (Set[A], rf.State)

      def init: (Set[A], rf.State) = (Set.empty, rf.init)
      def step(s: (Set[A], rf.State), a: A): (Set[A], rf.State) =
        (s._1 + a, if (s._1.contains(a)) s._2 else rf.step(s._2, a))
      def complete(s: (Set[A], rf.State)): B = rf.complete(s._2)
    }
  }
}

trait Converter[F[_]] {
  def converter[A]: Reducer[A, F[A]]
}

object Converter {
  def apply[F[_]: Converter]: Converter[F] = implicitly

  implicit val list: Converter[List] = new Converter[List] {
    def converter[A]: Reducer[A, List[A]] = Reducer.list
  }

  implicit val vector: Converter[Vector] = new Converter[Vector] {
    def converter[A]: Reducer[A, Vector[A]] = Reducer.vector
  }
}

@typeclass
trait Reducing[F[_]] {
  import Reducing._

  def reduce[A, B](fa: F[A], rf: Reducer[A, B]): B

  def transduce[A, B, C](fa: F[A], xf: Transducer[A, B], rf: Reducer[B, C]): C =
    reduce(fa, xf.trans(rf))

  def convert[G[_]: Converter, A](fa: F[A]): G[A] =
    reduce(fa, Converter[G].converter[A])

  def convertWith[G[_], A](fa: F[A]): ConvertWith[F, G, A] =
    new ConvertWith(this, fa)
}

object Reducing {
  class ConvertWith[F[_], G[_], A](reducing: Reducing[F], fa: F[A]) {
    def apply[B](xf: Transducer[A, B])(implicit G: Converter[G]): G[B] =
      reducing.transduce(fa, xf, G.converter[B])
  }

  implicit val list: Reducing[List] = new Reducing[List] {
    def reduce[A, B](fa: List[A], rf: Reducer[A, B]): B = {
      val it = fa.iterator
      var state = rf.init
      while (it.hasNext) {
        state = rf.step(state, it.next())
      }
      return rf.complete(state)
    }
  }

  implicit val vector: Reducing[Vector] = new Reducing[Vector] {
    def reduce[A, B](fa: Vector[A], rf: Reducer[A, B]): B = {
      val it = fa.iterator
      var state = rf.init
      while (it.hasNext) {
        state = rf.step(state, it.next())
      }
      return rf.complete(state)
    }
  }
}
