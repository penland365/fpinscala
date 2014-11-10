package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 3, datastructures")
    runExercise("Exercise 3.2", exercise33)
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

  private def exercise32(): Unit = {
    val l = List(1,2,3)
    println("Original List was --> " + l)
    println("l.tail --> " + List.tail(l))
  }

  private def exercise33(): Unit = {
    val l = List(1,2,3)
    println("Original list --> " + l)
    println("List.setHead(4) --> " + List.setHead(4, l))
  }


  private def runExercise(fnName: String, f: exercise): Unit = {
    println(fnName)
    f()
  }
}
