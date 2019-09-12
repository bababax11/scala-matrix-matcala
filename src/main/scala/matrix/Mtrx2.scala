package bababax11.matcala.matrix

import scala.collection.mutable.ArrayBuffer

class Mtrx2[T](val rows: Int, val cols: Int, val data: Array[T]) {

  if(data.length != rows * cols)
    throw new IndexOutOfBoundsException("サイズがあっていない")

  def this(rows: Int, data: Array[T]) = {
    this(rows, data.length / rows, data)
    if(data.length % rows != 0)
      throw new IndexOutOfBoundsException("不正なArrayとrowsの組み合わせ")
  }

  def apply(i: Int, j: Int): T = {
    data(i * cols + j)
  }

  override def equals(obj: Any): Boolean = obj match {
    case ob : Mtrx2[_] => this.data sameElements ob.data
    case _ => false
  }

  override def toString: String = {
    var s = "[\n"
    for(vec <- data) {
      s +=" [ "
      for(num <- vec) {
        s += num.toString + " "
      }
      s += "]\n"
    }
    s + "]"
  }
}

class ValueMtrx2[T](override val rows: Int, override val cols: Int, override val data: Array[T])
  extends Mtrx2[T](rows, cols, data) {

  def calcEach(mat: ValueMtrx2[T], f: (T,T) -> S): ValueMtrx2[S] = {
    if(rows != mat.rows || cols != mat.cols) {
      throw new IndexOutOfBoundsException("Shape is not same")
    }
    val arr = Array.ofDim(rows)
    for(i <- 0 to rows; vec <- data; otherVec <-mat.data) {
      val ar = Array.ofDim(cols)
      for(j <- 0 to cols; v <- vec; ov <- otherVec) {
          ar(j) = f(v, ov)
      }
      arr(i) = ar.toVector
    }
    val vec = arr.toVector
    new ValueMtrx2[Any](rows, cols, vec)
  }
}
