package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 5, Strictness and Laziness")
    runExercise("Exercise 5.1", exercise51)
  }

  private def exercise51(): Unit = {
    println("YOLO")
  }

  private def runExercise(fName: String, f: exercise): Unit = {
    println(fName)
    f()
  }
}
