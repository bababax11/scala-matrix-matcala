package bababax11.matcala.matrix
import org.scalatest._

class Mtrx2Test extends FlatSpec with DiagrammedAssertions{

  val mat = new Mtrx2(2,
    Array(1,2,3,
          4,5,6))
  val matSame = new Mtrx2(2,3,
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

  "access" should "be proper" in {
    assert(mat(0, 0) === 1)
    assert(mat(1, 2) === 6)
    intercept[IndexOutOfBoundsException](
      mat(0, 3)
    )
    intercept[IndexOutOfBoundsException](
      mat(3, 0)
    )
  }

//  val vmat = new ValueMtrx2[Int](2,
//    Array(1,2,3,
//          4,5,6))



}
