package ababax11.matcala.matrix

import bababax11.matcala.matrix.Mtrx2

object MtrxTestBenchMarks extends App {

  def printExecTime(process: => Unit): Unit = {
    val start = System.currentTimeMillis
    process
    println("処理時間： " + (System.currentTimeMillis - start) + " ミリ秒")
  }

  val mat = new Mtrx2(5, (1 to 10000000).toArray)
  val mat2 = new Mtrx2(5, (1 to 20000000 by 2).toArray)

  printExecTime {
    mat.calcEach(mat2)(_ + _)
  }

  printExecTime {
    mat.calcEachOneThread(mat2)(_ + _)
  }

  val mat0 = new Mtrx2(5, (1 to 10000).toArray)
  val mat02 = new Mtrx2(5, (1 to 20000 by 2).toArray)

  printExecTime {
    mat0.calcEach(mat02)(_ + _)
  }

  printExecTime {
    mat0.calcEachOneThread(mat02)(_ + _)
  }


}
