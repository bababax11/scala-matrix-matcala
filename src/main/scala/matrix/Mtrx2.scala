package bababax11.matcala.matrix

import scala.collection.mutable.ArrayBuffer

class Mtrx2[T](val rows: Int, val cols: Int, val data: Vector[Vector[T]]) {

  def this(data: Vector[Vector[T]]) = {
    this(data.length, if(data.length==0) 0 else data(0).length, data)
  }

  def apply(i: Int, j: Int): T = {
    data(i)(j)
  }

  override def equals(obj: Any): Boolean = obj match {
    case ob : Mtrx2[_] => this.data == ob.data
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

class ValueMtrx2[T](override val rows: Int, override val cols: Int, override val data: Vector[Vector[T]])
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
