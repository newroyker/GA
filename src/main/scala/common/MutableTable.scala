package common

import scala.collection.mutable.ArrayBuffer

case class MutableRow[T](size: Int) {
  private val data: ArrayBuffer[Option[T]] = ArrayBuffer.fill(size)(None)

  def set(p: Int, t: T): Unit = {
    assert(p >= 0 && p < size)
    data(p) = Some(t)
  }

  def get(p: Int): Option[T] = {
    assert(p >= 0 && p < size)
    data(p)
  }

  def apply(p: Int): T = get(p).get

  def fill(t: T): Unit = (0 until size).foreach(set(_, t))

  def max(implicit ord: Ordering[T]): T = data.flatten.max

  override def toString: String = s"(${(0 until size).map(get).map(_.getOrElse(" ")).mkString(",")})"
}

case class MutableTable[T](rows: Int, cols: Int) {

  private val data: ArrayBuffer[MutableRow[T]] = ArrayBuffer.fill(rows)(MutableRow[T](cols))

  def set(r: Int, c: Int, t: T): Unit = {
    assert(r >= 0 && r < rows)
    assert(c >= 0 && c < cols)
    data(r).set(c, t)
  }

  def get(r: Int, c: Int): Option[T] = {
    assert(r >= 0 && r < rows)
    assert(c >= 0 && c < cols)
    data(r).get(c)
  }

  def apply(r: Int, c: Int): T = get(r, c).get

  def fillRow(r: Int, t: T): Unit = {
    assert(r >= 0 && r < rows)
    data(r).fill(t)
  }

  def fillCol(c: Int, t: T): Unit = {
    assert(c >= 0 && c < cols)
    (0 until rows).foreach(r => data(r).set(c, t))
  }

  def max(implicit ord: Ordering[T]): T = data.map(_.max).max

  override def toString: String = s"[${(0 until rows).map(data(_).toString).mkString("\n ")}]"

}
