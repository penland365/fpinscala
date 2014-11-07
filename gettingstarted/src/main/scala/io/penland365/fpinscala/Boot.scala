package boot

object Boot {

  def main(args: Array[String]): Unit = {
    println("Hello, gettingstarted!")
    val i = factorial(7)
    println("Value of factorial was " + i)
  }

  private def factorial(n: Int): Int = {
    def loop(n: Int, acc: Int): Int =
      if(n <= 0) acc
      else loop(n-1, n*acc)

    loop(n, 1)
  }
}
