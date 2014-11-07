package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 2, gettingstarted")
    runExercise("Exercise 2.2", exercise22)
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

  private def isSorted[A](arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int, valid: Boolean): Boolean = {
      if(!valid || i >= arr.length) valid
      else if(i == 0) loop(i + 1, valid)
      else loop(i + 1, ordered(arr(i-1), arr(i)))
    }
    loop(0, true)
  }

  private def runExercise(fnName: String, f: exercise): Unit = {
    println(fnName)
    f()
  }

  private def exercise21(): Unit = {
    val i =  25
    println("fib(" + i + ") was --> " + fib(i))
  }

  private def exercise22(): Unit = {
    val iOrdered = new Function2[Int, Int, Boolean] {
      def apply(a: Int, b: Int) = a < b
    }
    val iArr = Array(1,2,3,4,5)
    println("Array[Int] sorted (should be true) --> " + isSorted(iArr, iOrdered))

    val sOrdered = new Function2[String, String, Boolean] {
      def apply(a: String, b: String) = a < b
    }
    val sArr = Array("a","k","c")
    println("Array[String] sorted (should be false) --> " + isSorted(sArr, sOrdered))
  }









}
