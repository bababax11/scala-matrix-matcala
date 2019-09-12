package bababax11.matcala.matrix
import org.scalatest._

class Mtrx2Test extends FlatSpec with DiagrammedAssertions{

  val mat = new Mtrx2(3,
    Array(1,2,3,
          4,5,6))
  val matSame = new Mtrx2(3,2,
    Array(1,2,3,
          4,5,6))

  "rows, cols" should "be the number of rows, cols" in {
    assert(mat.rows == 2)
    assert(mat.cols == 3)
  }
  "same matrix" should "be recognized as same" in {
    assert(mat == matSame)
  }
  "invalid matrix" should "throw Exception" in {
    intercept[IndexOutOfBoundsException](
      new Mtrx2(2, Array(1,2,3))
    )
    intercept[IndexOutOfBoundsException](
      new Mtrx2(2, 3, Array(1,2,3,4))
    )
  }

}
