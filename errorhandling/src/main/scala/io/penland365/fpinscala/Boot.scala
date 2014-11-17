package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 3, errorhandling")
    runExercise("Exercise 4.2", exercise42)
  }

  private def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  private def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map( x => math.pow(x - m, 2))))

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

  private def exercise42(): Unit = {
    val l = Seq(1.0,2.0,3.0,4.0,5.0)
    println("l = " + l)
    println("variance(l) = " + variance(l))
  }

  private def runExercise(fName: String, f: exercise): Unit = {
    println(fName)
    f()
  }
}
