package bababax11.matcala.matrix

import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import scala.math.min

// 以下はFutureを使うため
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

//import scala.util.control.Breaks

class Mtrx2[T](val rows: Int, val cols: Int, val data: Array[T]) {

  if (data.length != rows * cols)
    throw new IndexOutOfBoundsException(s"サイズがあっていない ${data.length}, ${(rows, cols)}")

  def this(rows: Int, data: Array[T]) = {
    this(rows, data.length / rows, data)
    if(data.length % rows != 0)
      throw new IndexOutOfBoundsException(s"不正なArrayとrowsの組み合わせ ${data.length}, $rows")
  }

  def apply(i: Int, j: Int): T = {
    if (j >= cols || j < 0)
      throw new IndexOutOfBoundsException(s"j must be cols: $cols")
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

  def calcEach[S : ClassTag, U >: T](otherMat: Mtrx2[U])(f: (U, U) => S): Mtrx2[S] = {
    if (rows != otherMat.rows || cols != otherMat.cols || rows * cols <= 0) {
      throw new IndexOutOfBoundsException(s"Shape is not same: ${(rows, cols)} and ${(otherMat.rows, otherMat.cols)}")
    }
    val N = rows * cols / 6 // スレッド1つあたりの要素数。動かしてみて決める必要あり
    val r = rows * cols % N
    val nDiv = if (r!=0) rows * cols / N + 1 else rows * cols / N
    val arrFuture = Array.ofDim[Future[Array[S]]](nDiv)
    for (l <- 0 until nDiv) {
      arrFuture(l) = Future {
        val ar = Array.ofDim[S](if (l != nDiv-1 || r==0) N else r)
        for(k <- N*l until min(N*(l+1), rows*cols)) {
          ar(k-N*l) = f(data(k), otherMat.data(k))
        }
        ar
      }
    }
    var array = Array.ofDim[S](0)
    for (l <- 0 until nDiv) {
      val result = Await.result(arrFuture(l), Duration.Inf) // スコープは内部
      array = Array.concat(array, result)
    }
    new Mtrx2[S](rows, cols, array)
  }

  def calcEachOneThread[S : ClassTag, U >: T](otherMat: Mtrx2[U])(f: (U, U) => S): Mtrx2[S] = {
    if (rows != otherMat.rows || cols != otherMat.cols || rows * cols <= 0) {
      throw new IndexOutOfBoundsException(s"Shape is not same: ${(rows, cols)} and ${(otherMat.rows, otherMat.cols)}")
    }
    val arr = Array.ofDim[S](rows*cols)
    for (k <- 0 until rows*cols)
      arr(k) = f(data(k), otherMat.data(k))
    new Mtrx2[S](rows, cols, arr)
  }


  def map[S: ClassTag, U >: T](f: U => S): Mtrx2[S] = {
    val N = rows * cols / 6 // スレッド1つあたりの要素数。動かしてみて決める必要あり
    val r = rows * cols % N
    val nDiv = if (r!=0) rows * cols / N + 1 else rows * cols / N
    val arrFuture = Array.ofDim[Future[Array[S]]](nDiv)
    for (l <- 0 until nDiv) {
      arrFuture(l) = Future {
        val ar = Array.ofDim[S](if (l != nDiv-1 || r==0) N else r)
        for(k <- N*l until min(N*(l+1), rows*cols)) {
          ar(k-N*l) = f(data(k))
        }
        ar
      }
    }
    var array = Array.ofDim[S](0)
    for (l <- 0 until nDiv) {
      val result = Await.result(arrFuture(l), Duration.Inf) // スコープは内部
      array = Array.concat(array, result)
    }
    new Mtrx2[S](rows, cols, array)
  }
}

class ValueMtrx2[T: ClassTag](override val rows: Int, override val cols: Int, override val data: Array[T])(implicit num: Numeric[T])
  extends Mtrx2[T](rows, cols, data) {

  def this(rows: Int, data: Array[T])(implicit num: Numeric[T]) = {
    this(rows, data.length / rows, data)
    if(data.length % rows != 0)
      throw new IndexOutOfBoundsException(s"不正なArrayとrowsの組み合わせ ${data.length}, $rows")
  }

  def calcEachNumeric[U >: T : ClassTag](otherMat: ValueMtrx2[U])(f: (U, U) => U)(implicit num: Numeric[U]): ValueMtrx2[U] = {
    if (rows != otherMat.rows || cols != otherMat.cols) {
      throw new IndexOutOfBoundsException(s"Shape is not same: ${(rows, cols)} and ${(otherMat.rows, otherMat.cols)}")
    }
    val N = rows * cols / 6 // スレッド1つあたりの要素数。動かしてみて決める必要あり
    val r = rows * cols % N
    val nDiv = if (r!=0) rows * cols / N + 1 else rows * cols / N
    val arrFuture = Array.ofDim[Future[Array[U]]](nDiv)
    for (l <- 0 until nDiv) {
      arrFuture(l) = Future {
        val ar = Array.ofDim[U](if (l != nDiv-1 || r==0) N else r)
        for(k <- N*l until min(N*(l+1), rows*cols)) {
          ar(k-N*l) = f(data(k), otherMat.data(k))
        }
        ar
      }
    }
    var array = Array.ofDim[U](0)
    for (l <- 0 until nDiv) {
      val result = Await.result(arrFuture(l), Duration.Inf) // スコープは内部
      array = Array.concat(array, result)
    }
    new ValueMtrx2[U](rows, cols, array)
  }

  def mapNumeric[U >: T: ClassTag, S: ClassTag](f: U => S)(implicit num: Numeric[S]): ValueMtrx2[S] = {
    val N = rows * cols / 6 // スレッド1つあたりの要素数。動かしてみて決める必要あり
    val r = rows * cols % N
    val nDiv = if (r!=0) rows * cols / N + 1 else rows * cols / N
    val arrFuture = Array.ofDim[Future[Array[S]]](nDiv)
    for (l <- 0 until nDiv) {
      arrFuture(l) = Future {
        val ar = Array.ofDim[S](if (l != nDiv-1 || r==0) N else r)
        for(k <- N*l until min(N*(l+1), rows*cols)) {
          ar(k-N*l) = f(data(k))
        }
        ar
      }
    }
    var array = Array.ofDim[S](0)
    for (l <- 0 until nDiv) {
      val result = Await.result(arrFuture(l), Duration.Inf) // スコープは内部
      array = Array.concat(array, result)
    }
    new ValueMtrx2[S](rows, cols, array)
  }

  def +[U >: T: ClassTag](otherMat: ValueMtrx2[U])(implicit num: Numeric[U]): ValueMtrx2[U] =
    calcEachNumeric(otherMat)( (x: U, y: U) => num.plus(x, y) )

  def -[U >: T: ClassTag](otherMat: ValueMtrx2[U])(implicit num: Numeric[U]): ValueMtrx2[U] =
    calcEachNumeric(otherMat)( (x: U, y: U) => num.minus(x, y) )

  def *[U >: T: ClassTag](otherMat: ValueMtrx2[U])(implicit num: Numeric[U]): ValueMtrx2[U] =
    calcEachNumeric(otherMat)( (x: U, y: U) => num.times(x, y) )

  def +[U >:T: ClassTag](x: U)(implicit num: Numeric[U]): ValueMtrx2[U] =
    mapNumeric( (z: U) => num.plus(z, x) )

  def -[U >:T: ClassTag](x: U)(implicit num: Numeric[U]): ValueMtrx2[U] =
    mapNumeric( (z: U) => num.minus(z, x) )

  def *[U >:T: ClassTag](x: U)(implicit num: Numeric[U]): ValueMtrx2[U] =
    mapNumeric( (z: U) => num.times(z, x) )


}
