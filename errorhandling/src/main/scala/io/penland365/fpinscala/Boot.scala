package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 3, errorhandling")
    runExercise("Exercise 4.8", exercise48)
  }

  private def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  private def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map( x => math.pow(x - m, 2))))

  private def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = 
    a.flatMap(aa => b map(bb => f(aa, bb)))

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

  private def exercise44(): Unit = {
    val x = List(Some(1), Some(2), Some(3))
    val xs = List(Some(1), Some(2), None)
    println("x = " + x)
    println("Option.sequence(x) = " + Option.sequence(x))
    println("xs = " + xs)
    println("Option.sequence(xs) = " + Option.sequence(xs))
  }

  private def exercise46(): Unit = {
    val right = Right(71) 

    println("right = " + right)

    println("right.map(x => x + 1) = " + right.map(x => x + 1))
    println("right.flatMap(x => Either(x + 1)) = " + right.flatMap(x => Right(x + 1)))
  }

  private def exercise48(): Unit = {
    var answer = "The simplest course would be to implement a new data type that lests us keep a list of errors in the data constructor, like so:\n\n"
    answer += "trait Partial[+A,+B]\n"
    answer += "case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]\n"
    answer += "case class Success[+B](get: B) extends Partial[Nothing, B]"
    println(answer)
  }

  private def runExercise(fName: String, f: exercise): Unit = {
    println(fName)
    f()
  }
}
