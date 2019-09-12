package bababax11.matcala.matrix

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class Mtrx2[T](val rows: Int, val cols: Int, val data: Array[T]) {

  if (data.length != rows * cols)
    throw new IndexOutOfBoundsException(s"サイズがあっていない ${data.length}, ${(rows, cols)}")

  def this(rows: Int, data: Array[T]) = {
    this(rows, data.length / rows, data)
    if(data.length % rows != 0)
      throw new IndexOutOfBoundsException(s"不正なArrayとrowsの組み合わせ ${data.length}, $rows")
  }

  def apply(i: Int, j: Int): T = {
    if (j >= cols)
      throw new IndexOutOfBoundsException(s"j must be less than cols: $cols")
    data(i * cols + j)
  }

  override def equals(obj: Any): Boolean = obj match {
    case ob : Mtrx2[_] =>
      rows == ob.rows && cols == ob.cols && (this.data sameElements ob.data)
    case _ => false
  }

  override def toString: String = {
    var s = "[\n"
    for (k <- 0 until rows*cols) {
      if (k % cols == 0) s += " [ "
      s += data(k).toString + " "
      if (k % cols == cols-1) s += "]\n"
    }
    s + "]"
  }
}

class ValueMtrx2[T <: AnyVal](override val rows: Int, override val cols: Int, override val data: Array[T])
  extends Mtrx2[T](rows, cols, data) {

  def calcEach[S <: AnyVal: ClassTag, U >: T <: AnyVal](otherMat: ValueMtrx2[U], f: (U, U) => S): ValueMtrx2[S] = {
    if (rows != otherMat.rows || cols != otherMat.cols) {
      throw new IndexOutOfBoundsException(s"Shape is not same: ${(rows, cols)} and ${(otherMat.rows, otherMat.cols)}")
    }
    val arr = Array.ofDim[S](cols*rows)
    for (k <- 0 until rows*cols) {
      arr(k) = f(data(k), otherMat.data(k))
    }
    new ValueMtrx2[S](rows, cols, arr)
  }

//  def +[U >: T <: AnyVal](otherMat: ValueMtrx2[U]) = {
//    calcEach(otherMat, (x, y): (T, T) => x + y )
//  }
}
