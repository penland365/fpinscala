package boot

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z:B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Nil list")
    case Cons(_, x) => x
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case _   => Cons(h, List.tail(l))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init0[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h, init0(t))
  }

  def init1[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def loop(l: List[A]): List[A] = l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; loop(t)
    }

    loop(l)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  def sumFoldLeft(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)
  def productFoldLeft(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)
  def lengthFoldLeft[A](xs: List[A]): Int = foldLeft(xs, 0)((acc, _) => acc + 1)

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List[A]())((acc,h) => Cons(h, acc))

  def foldLeftByFoldRight[A,B](xs: List[A], z: B)(f: (B, A) => B): B =
    foldRight(xs, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  def foldRightByFoldLeft[A,B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(xs), z)((b,a) => f(a,b))

  def appendByFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concatenate[A](xs: List[List[A]]): List[A] =
    foldRight(xs, Nil:List[A])(append)

  def add1ToEachElem(x: List[Int]): List[Int] =
    foldRight(x, Nil:List[Int])((h,t) => Cons(h + 1, t))

  def transformListDoubleToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil:List[String])((h,t) => Cons(h.toString, t))

  def map[A,B](xs: List[A])(f: A => B): List[B] =
    foldRight(xs, Nil:List[B])((h,t) => Cons(f(h), t))

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    foldRight(xs, Nil:List[A])((h,t) => if(f(h)) Cons(h,t) else t)

  def flatMap[A,B](xs: List[A])(f: A => List[B]): List[B] =
    //foldRight(xs, Nil:List[B])((h,t) => append(f(h), t))
    concatenate(map(xs)(f))

  def filterViaFlatMap[A](xs: List[A])(f: A => Boolean): List[A] =
    flatMap(xs)(a => if(f(a)) List(a) else Nil)

  def addPairwise(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1, t2))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
}













