package codes.quine.labo
package dali

sealed trait HList extends Serializable with Product

sealed trait HNil extends HList {
  def :*:[H](h: H): H :*: HNil = dali.:*:(h, this)
  override def toString: String = "HNil"
}

case object HNil extends HNil

final case class :*:[+H, +T <: HList](head: H, tail: T) extends HList {
  override def toString: String = s"$head :*: $tail"
}

object HList {
  trait Select[L <: HList] {
    type Result
    def select(l: L): Result
  }

  object Select extends SelectInstances0 {
    type Aux[L <: HList, R] = Select[L] { type Result = R }
  }

  private[dali] trait SelectInstances0 extends SelectInstances1 {
    implicit def selectHead[H, T <: HList]: Select.Aux[H :*: T, H] = new Select[H :*: T] {
      type Result = H
      def select(l: H :*: T): H = l.head
    }
  }

  private[dali] trait SelectInstances1 {
    implicit def selectTail[H, T <: HList, R](implicit s: Select.Aux[T, R]): Select.Aux[H :*: T, R] =
      new Select[H :*: T] {
        type Result = R
        def select(l: H :*: T): R = s.select(l.tail)
      }
  }

  trait ToHlistOps {
    implicit class HListOps[L <: HList](l: L) {
      def :*:[H](h: H): H :*: L = dali.:*:(h, l)
      def select[R](implicit s: Select.Aux[L, R]): R = s.select(l)
    }
  }

  object ops extends ToHlistOps
}
