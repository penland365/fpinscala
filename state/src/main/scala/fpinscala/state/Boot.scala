package fpinscala.state

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 6, Purely functional state")
    runExercise("Exercise 6.1", exercise61)
  }

  private def exercise61(): Unit = {
    val rng = SimpleRNG(42)
    val(n1, rng2) = rng.nextInt
    println(n1)
  }

  private def runExercise(fnName: String, f: exercise): Unit = {
    println(fnName)
    f()
  }
}
