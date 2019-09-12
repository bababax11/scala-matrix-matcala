package bababax11.matcala.matrix

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
    for(array <- data) {
      s +=" [ "
      for(num <- array) {
        s += num.toString + " "
      }
      s += "]\n"
    }
    s + "]"
  }
}
