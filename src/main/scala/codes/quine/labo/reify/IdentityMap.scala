package codes.quine.labo
package reify

import java.util.IdentityHashMap
import scala.collection.MapFactory
import scala.collection.mutable

private[reify] class IdentityMap[K, V](val inner: IdentityHashMap[K, V]) extends Map[K, V] {
  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    val it = inner.entrySet.iterator
    def hasNext: Boolean = it.hasNext
    def next(): (K, V) = {
      val n = it.next()
      (n.getKey, n.getValue)
    }
  }

  def get(key: K): Option[V] = if (inner.containsKey(key)) Some(inner.get(key)) else None

  def removed(key: K): IdentityMap[K, V] = {
    val newInner = inner.clone.asInstanceOf[IdentityHashMap[K, V]]
    newInner.remove(key)
    new IdentityMap(newInner)
  }

  def updated[U >: V](key: K, value: U): IdentityMap[K, U] = {
    val newInner = inner.clone.asInstanceOf[IdentityHashMap[K, U]]
    newInner.put(key, value)
    new IdentityMap(newInner)
  }
}

object IdentityMap extends MapFactory[IdentityMap] {
  def empty[K, V]: IdentityMap[K, V] = new IdentityMap(new IdentityHashMap())

  def from[K, V](it: IterableOnce[(K, V)]): IdentityMap[K, V] = {
    val inner = new IdentityHashMap[K, V]
    it.iterator.foreach { case (k, v) => inner.put(k, v) }
    new IdentityMap(inner)
  }

  def newBuilder[K, V]: mutable.Builder[(K, V), IdentityMap[K, V]] = new mutable.Builder[(K, V), IdentityMap[K, V]] {
    val inner = new IdentityHashMap[K, V]
    def clear(): Unit = inner.clear()
    def addOne(elem: (K, V)): this.type = {
      val (k, v) = elem
      inner.put(k, v)
      this
    }
    def result(): IdentityMap[K, V] = new IdentityMap(inner)
  }
}
