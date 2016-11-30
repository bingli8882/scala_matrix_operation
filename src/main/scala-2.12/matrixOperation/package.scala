/**
  * Created by bing on 11/29/16.
  */
package object matrixOperation {

  def timer[A](func: => A):A={
    val t0 = System.nanoTime()
    val result = func
    val t1 = System.nanoTime()
    println(s"Time: ${t1-t0} ns")
    result
  }
}
