package codes.quine.labo
package transducer

import simulacrum.typeclass
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
