package boot

object Boot {

  def main(args: Array[String]): Unit = {
    println("This is Chapter 2, gettingstarted")
    runExercise21
  }

  private def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(currMinus2: Int, currMinus1: Int, curr:Int, n: Int): Int = {
      val fib = currMinus2 + currMinus1
      if(curr == n ) fib
      else loop(currMinus1, fib, curr + 1, n)
    }

    if( n < 0) return -1
    else if( n == 0) return 0
    else if (n == 1) return 1
    else loop(0, 1, 2, n)
  }

  private def factorial(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int =
      if(n <= 0) acc
      else loop(n-1, n*acc)

    loop(n, 1)
  }

  private def runExercise21(): Unit = {
    println("Running Exercise 2.1, a recursive function for the nth Fibonacci number")
    val i =  25 
    println("fib(" + i + ") was --> " + fib(i))
  }
}
