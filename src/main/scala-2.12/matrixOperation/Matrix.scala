package matrixOperation

/**
  * Created by bing on 11/29/16.
  */
class Matrix(val columnNumber: Int, val rowNumber: Int, val elements: Array[Double]) {

  def this(columnNumber: Int, rowNumber: Int) = this(columnNumber, rowNumber, new Array[Double](columnNumber * rowNumber))

  def apply(x: Int, y: Int) = elements(y * columnNumber + x)

  def update(x: Int, y: Int, n: Double) = elements(y * columnNumber + x) = n

  private def maxNumberLength = elements.map(_.toString.length).max

  def printOut = {
    val max = maxNumberLength
    (0 until rowNumber).foreach(y => {
      print('|')
      (0 until columnNumber).foreach(x => {
        printf(s"%${max + 1}s", this (x, y).toString)
      })
      println('|')
    })
  }

  private def mAndP(other: Matrix, op: (Double, Double) => Double): Matrix = {
    if (this.columnNumber != other.columnNumber || this.rowNumber != other.rowNumber) throw new UnexpectedMatrixSizeException
    new Matrix(this.columnNumber, this.rowNumber, this.elements.zip(other.elements).map { case (x, y) => op(x, y) })
  }

  def +(other: Matrix): Matrix = mAndP(other, _ + _)

  def -(other: Matrix): Matrix = mAndP(other, _ - _)

  def *(other: Matrix): Matrix = {
    if (this.columnNumber != other.rowNumber) throw new UnexpectedMatrixSizeException
    val e = new Array[Double](this.rowNumber * other.columnNumber)
    (0 until e.length).foreach(i => {
      val y = i / other.columnNumber
      val x = i % other.columnNumber
      e(i) = (0 until this.columnNumber).map(z => {
        this (z, y) * other(x, z)
      }).foldRight(0.0)(_ + _)
    })
    new Matrix(other.columnNumber, this.rowNumber, e)
  }


}

object Matrix {

  def IMatrix(n: Int) = {
    val result = new Matrix(n, n, new Array[Double](n * n))
    (0 until n).foreach(x => result(x, x) = 1)
    result
  }

  def main(args: Array[String]): Unit = {
    val m = new Matrix(3, 4)
    m(0, 0) = 1
    m(1, 1) = 5
    m(2, 3) = 6
    m.printOut

    println()

    val m1 = Matrix.IMatrix(3)
    m1.printOut

    println()

    m * m1 printOut

    println()

    m1 - m1 printOut


  }
}
