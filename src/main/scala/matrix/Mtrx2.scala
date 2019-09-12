package bababax11.matcala.matrix

import scala.collection.mutable.ArrayBuffer

class Mtrx2[+T](val rows: Int, val cols: Int, val data: Array[T]) {

  if (data.length != rows * cols)
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
    case ob : Mtrx2[_] =>
      rows == ob.rows && cols == ob.cols && (this.data sameElements ob.data)
    case _ => false
  }

  override def toString: String = {
    var s = "[\n"
    for (k <- 0 to rows*cols; v <- data) {
      if (k % cols == 0) s += " ["
      s += v.toString + " "
      if (k % cols == cols-1) s += "]\n"
    }
    s + "]"
  }
}

class ValueMtrx2[+T <: AnyVal](override val rows: Int, override val cols: Int, override val data: Array[T])
  extends Mtrx2[T](rows, cols, data) {

  def calcEach[S <: AnyVal, U >: T](otherMat: ValueMtrx2[U], f: (U, U) => S): ValueMtrx2[S] = {
    if (rows != otherMat.rows || cols != otherMat.cols) {
      throw new IndexOutOfBoundsException(s"Shape is not same: ${(rows, cols)} and ${(otherMat.rows, otherMat.cols)}")
    }
    val arr = Array.ofDim[S](rows * cols)
    for (k <- 0 to rows*cols) {
      arr(k) = f(data(k), otherMat.data(k))
    }
    new ValueMtrx2[S](rows, cols, arr)
  }
}
