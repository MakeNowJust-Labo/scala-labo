package codes.quine.labo
package transducer

import scala.collection.mutable

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
