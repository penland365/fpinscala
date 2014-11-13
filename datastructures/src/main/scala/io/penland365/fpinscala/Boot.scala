package boot

object Boot {

  type exercise = () => Unit

  def main(args: Array[String]): Unit = {
    println("This is Chapter 3, datastructures")
    runExercise("Exercise 3.15", exercise315)
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

  private def exercise34(): Unit = {
    val l = List(1,2,3,4,5)
    println("Original list --> " + l)
    println("List.drop(3) --> " + List.drop(l, 2))
  }

  private def exercise35(): Unit = {
    val l = List(2,4,6,7,9)
    println("Original list --> " + l)
    println("List.dropWhile(n % 2 == 0) --> " + List.dropWhile(l, (n: Int) => n % 2 == 0))
  }

  private def exercise36(): Unit = {
    val l = List(1,2,3,4,5)
    println("Original list --> " + l)
    println("List.init0 --> " + List.init0(l))
    println("List.init1 --> " + List.init1(l))
  }

  private def exercise37(): Unit = {
    println("No; before we call the function `f` we traverse the entire list")
  }

  private def exercise38(): Unit = {
    val xs = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))
    println(xs)
  }

  private def exercise39(): Unit = {
    val xs = List(1,2,3,4,5)
    println("Original list ---> " + xs)
    println("List.length --> " + List.length(xs))
  }

  private def exercise310(): Unit = {
    val xs = List(1,2,3,4,5)
    println("Original list --> " + xs)
    println("List.foldLeft --> " + List.foldLeft(xs, 0)(_ + _))
  }

  private def exercise311(): Unit = {
    val l = List(1,2,3,4,5)
    val ll = List(1.0,2.0,3.0,4.0,5.0)
    println("val l = " + l)
    println("List.sumFoldLeft(l) = " + List.sumFoldLeft(l))
    println("List.productFoldLeft(l) = " + List.productFoldLeft(ll))
    println("List.lengthFoldLeft(l) = " + List.lengthFoldLeft(l))
  }

  private def exercise312(): Unit = {
    val l = List(1,2,3)
    println("val l = " + l)
    println("List.reverse(l) = " + List.reverse(l))
  }

  private def exercise313(): Unit = {
    val l = List(1,2,3)
    println("val l = " + l)
    val i = List.foldLeftByFoldRight(l, 0)(_ + _)
    val j = List.foldRightByFoldLeft(l, 0)(_ + _)
    println("List.foldLeftByFoldRight(l, 0)(_ + _) = " + i)
    println("List.foldRightByFoldLeft(l, 0)(_ + _) = " + j)
  }

  private def exercise314(): Unit = {
    val a1 = List(1,2)
    val a2 = List(3,4)
    println("a1 = " + a1)
    println("a2 = " + a2)
    println("List.appendByFoldRight(a1, a2) = " + List.appendByFoldRight(a1, a2))
  }

  private def exercise315(): Unit = {
    val a1 = List(1,2,3)
    val a2 = List(3,4,5)
    val a3 = List(6,7,8)
    val xs = List(a1, a2, a3)
    println("a1 = " + a1)
    println("a2 = " + a2)
    println("a3 = " + a3)
    println("xs = " + xs)
    println("List.concatenate(xs) = " + List.concatenate(xs))
  }

  private def runExercise(fnName: String, f: exercise): Unit = {
    println(fnName)
    f()
  }
}
