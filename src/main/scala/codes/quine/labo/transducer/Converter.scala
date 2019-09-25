package codes.quine.labo
package transducer

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
