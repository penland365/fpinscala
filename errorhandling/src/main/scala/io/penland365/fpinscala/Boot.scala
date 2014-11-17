package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 3, errorhandling")
    runExercise("Exercise 4.1", exercise41)
  }

  private def exercise41(): Unit = {
    val o = Some(71)
    val n = None

    println("o = Some(71)")
    println("n = None")

    println("o.map(x => x + 1) = " + o.map(x => x + 1))
    println("o.flatMap(x => Some(x + 1)) = " + o.flatMap(x => Some(x + 1)))
    println("o.getOrElse(17) = " + o.getOrElse(17))
    println("n.getOrElse(17) = " + n.getOrElse(17))
  }

  private def runExercise(fName: String, f: exercise): Unit = {
    println(fName)
    f()
  }
}
