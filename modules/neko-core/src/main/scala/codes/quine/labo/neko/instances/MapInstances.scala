package codes.quine.labo
package neko
package instances

import scala.annotation.tailrec
import syntax._

trait MapInstances extends MapInstances0

private[instances] trait MapInstances0 {
  implicit val mapComposeInstance: Compose[Map] = new Compose[Map] {
    def compose[A, B, C](f: Map[B, C], g: Map[A, B]): Map[A, C] = g.flatMap { case (a, b) => f.get(b).map((a, _)) }
    override def andThen[A, B, C](f: Map[A, B], g: Map[B, C]): Map[A, C] = f.flatMap {
      case (a, b) => g.get(b).map((a, _))
    }
  }

  implicit def mapFlatMapInstance[K]: FlatMap[Map[K, *]] = new FlatMap[Map[K, *]] {
    def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.map { case (k, a) => (k, f(a)) }
    override def flatMap[A, B](fa: Map[K, A])(f: A => Map[K, B]): Map[K, B] =
      fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }

    def tailRecM[A, B](a: A)(f: A => Map[K, Either[A, B]]): Map[K, B] = {
      val buf = Map.newBuilder[K, B]

      @tailrec def loop(k: K, e: Either[A, B]): Unit =
        e match {
          case Left(a) =>
            f(a).get(k) match {
              case Some(e1) => loop(k, e1)
              case None     => ()
            }
          case Right(b) => buf += ((k, b))
        }

      f(a).foreach { case (k, e) => loop(k, e) }
      buf.result
    }
  }

  implicit def mapEqInstance[K, A: Eq]: Eq[Map[K, A]] = new Eq[Map[K, A]] {
    def eqv(x: Map[K, A], y: Map[K, A]): Boolean =
      x.size == y.size && x.forall {
        case (k, xv) =>
          y.get(k) match {
            case Some(yv) => xv === yv
            case None     => false
          }
      }
  }

  implicit def mapHashInstances[K, A: Hash]: Hash[Map[K, A]] = new Hash[Map[K, A]] {
    def eqv(x: Map[K, A], y: Map[K, A]): Boolean = mapEqInstance[K, A].eqv(x, y)
    def hash(x: Map[K, A]): Int = x.foldLeft("Hash".hash)((h, kv) => h + kv._1.hashCode * 31 + kv._2.hash)
  }
}

package object map extends MapInstances
