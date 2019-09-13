package bababax11.matcala.matrix
import org.scalatest._

class Mtrx2Test extends FlatSpec with DiagrammedAssertions{

  val mat = new Mtrx2(2,
    Array(1,2,3,
          4,5,6))
  val matSame = new Mtrx2(2,3,
    Array(1,2,3,
          4,5,6))

  val matDiff = new Mtrx2(1,
    Array(1,2,3,4,5,6))

  "rows, cols" should "be the number of rows, cols" in {
    assert(mat.rows == 2)
    assert(mat.cols == 3)
  }

  "same matrix" should "be recognized as same" in {
    assert(mat == matSame)
    assert(mat != matDiff)
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
      mat(2, 0)
    )
  }

  val mat2 = new Mtrx2(2, Array(-1,-2,-3,-4,-5,0))

  "calcEach" should "be mapping" in {
    assert(mat.calcEach(mat2)(_ + _) ==
            new Mtrx2(2, Array(0,0,0,0,0,6)))
  }

  val vMat = new ValueMtrx2[Int](2,
    Array[Int](1,2,3,
          4,5,6))

  val vMat2 = new ValueMtrx2[Int](2, Array(1,2,3,4,4,0))

  "calculation" should "be done properly" in {

    assert(vMat +[Int] vMat2 == new ValueMtrx2[Int](2, Array(2,4,6,8,9,6)))
    assert(vMat -[Int] vMat2 == new ValueMtrx2[Int](2, Array(0,0,0,0,1,6)))
    assert(vMat *[Int] vMat2 == new ValueMtrx2[Int](2, Array(1,4,9,16,20,0)))
    assert(vMat +[Int] 1 == new ValueMtrx2[Int](2, Array(2,3,4,5,6,7)))
    assert(vMat -[Int] 1 == new ValueMtrx2[Int](2, Array(0,1,2,3,4,5)))
    assert(vMat *[Int] 2 == new ValueMtrx2[Int](2, Array(2,4,6,8,10,12)))
  }





}
