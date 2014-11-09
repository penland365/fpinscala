package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 3, datastructures")
    runExercise("Exercise 3.1", exercise31)
  }

  private def exercise31(): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case _ => 101
    }
    println(x)
  }

  private def runExercise(fnName: String, f: exercise): Unit = {
    println(fnName)
    f()
  }
}
