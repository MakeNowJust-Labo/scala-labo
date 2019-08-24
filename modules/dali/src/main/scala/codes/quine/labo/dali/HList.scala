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
  trait ToHlistOps {
    implicit class HListOps[L <: HList](l: L) {
      def :*:[H](h: H): H :*: L = dali.:*:(h, l)
    }
  }

  object ops extends ToHlistOps
}
