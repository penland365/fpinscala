package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 2, gettingstarted")
    runExercise("Exercise 2.5", exercise25)
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

  private def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a,b)

  private def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a,b) => f(a)(b)

  private def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  private def runExercise(fnName: String, f: exercise): Unit = {
    println(fnName)
    f()
  }

  private def exercise21(): Unit = {
    val i =  25
    println("fib(" + i + ") was --> " + fib(i))
  }

  private def exercise22(): Unit = {
    val iStatement = "Array[Int] sorted ( should be true ) --> "
    println(iStatement + isSorted(Array(1,2,3), (a: Int, b: Int) => a < b ))

    val sStatement = "Array[String] sorted (should be false) --> "
    println(sStatement + isSorted(Array("a","z","c"), (a: String, b: String) => a < b))
  }

  private def exercise23(): Unit = {
    println("Implement def curry[A,B,C](f: (A,B) => C): A=> (B => C)") 
    println("There is only one implementation that compiles")
  }

  private def exercise24(): Unit = {
    println("Implement def uncurry[A,B,C](f: A => B => C): (A,B) => C") 
    println("There is only one implementation that compiles")
  }

  private def exercise25(): Unit = {
    println("Implement def compose[A,B,C](f: B => C, g: A => B): A => C") 
    println("There is only one implementation that compiles")
  }





}
