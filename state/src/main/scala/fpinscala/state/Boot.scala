package fpinscala.state

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 6, Purely functional state")
    runExercise("Exercise 6.4", exercise64)
  }

  private def exercise61(): Unit = {
    val(n1, rng2) = RNG.nonNegativeInt(SimpleRNG(71))
    println(n1)
  }

  private def exercise62(): Unit = {
    val t = RNG.double(SimpleRNG(19))
    println(t._1)
  }

  private def exercise63(): Unit = {
    val i = RNG.intDouble(SimpleRNG(11))
    println("intDouble => (" + i._1._1 + ", " + i._1._2 + ")")

    val j = RNG.doubleInt(SimpleRNG(13))
    println("doubleInt=> (" + j._1._1 + ", " + j._1._2 + ")")

    val k = RNG.double3(SimpleRNG(17))
    println("double3=> (" + k._1._1 + ", " + k._1._2 + ", " + k._1._3 + ")")
  }

  private def exercise64(): Unit = {
    val(l, rng) = RNG.ints(7)(SimpleRNG(23))
    l.foreach(x => println(x))
  }

  private def runExercise(fnName: String, f: exercise): Unit = {
    println(fnName)
    f()
  }
}
