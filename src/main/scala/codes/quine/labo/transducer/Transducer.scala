package codes.quine.labo
package transducer

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
