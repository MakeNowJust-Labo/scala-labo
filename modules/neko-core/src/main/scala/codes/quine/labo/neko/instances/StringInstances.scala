package codes.quine.labo
package neko
package instances

trait StringInstances extends StringInstances0

trait StringInstances0 extends StringInstances1 {
  implicit val stringMonoidInstance: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def concat(x: String, y: String): String = x + y
  }

  implicit val stringOrdInstance: Ord[String] = Ord.fromOrdering[String]
}

trait StringInstances1 {
  implicit val stringHashInstance: Hash[String] = new Hash[String] {
    def eqv(x: String, y: String): Boolean = x == y
    def hash(x: String): Int = x.hashCode
  }
}

package object string extends StringInstances
