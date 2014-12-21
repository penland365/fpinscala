package io.ptx.fpinscala

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => buf += h(); loop(t())
      case _ => buf.toList
    }

    loop(this)
  }

  def take(n: Int): Stream[A] = {
    if(n > 0) this match {
      case Cons(h, t) if n == 1 => Stream.cons(h(), Stream.empty)
      case Cons(h, t) => Stream.cons(h(), t().take(n-1))
      case _ => Stream.empty

    }
    else Stream()
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(i: Int, s: Stream[A]): Stream[A] = {
      if(i <= 0) s
      else s match {
        case Cons(h,t) => loop(i-1, t())
        case _ => Stream()
      }
    }

    loop(n, this)
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => Stream.cons(h(), t() takeWhile f)
    case _ => Stream.empty
  }

  def takeWhileByFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) =>
        if(f(h)) Stream.cons(h,t)
        else Empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _         => z
  }

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def headOptionByFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def mapByFoldRight[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))

  def filterByFoldRight[B](f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => {
      if(f(h)) Stream.cons(h,t)
      else t
    })

  def appendByFoldRight[B >: A](s: Stream[B]): Stream[B] = 
    foldRight(s)((h,t) => Stream.cons(h,t))

  def flatMapByFoldRight[B](f: A => Stream[B]): Stream[B] = 
    foldRight(Stream.empty[B])((h, t) => f(h).appendByFoldRight(t))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def effecientConstant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  val fibs = {
    def go(x: Int, y: Int): Stream[Int] =
      Stream.cons(x, go(y, x + y))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None        => Stream.empty
    case Some((h,s)) => Stream.cons(h, unfold(s)(f))
  }

  val fibsByUnfold = {
    def fib(x: (Int, Int)): Option[(Int, (Int, Int))] = Some(x._2, (x._2, x._1 + x._2))
    unfold((0,1))(fib)
  }

  def fromByUnfold(n: Int): Stream[Int] = 
    unfold(n)(n => Some((n, n+1)))

  def constantByUnfold[A](a: A): Stream[A] = 
    unfold(a)(_ => Some((a, a)))

  def onesByUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

  def onesByConstantByUnfold: Stream[Int] = constantByUnfold(1)

  def mapByUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some((f(h()), t()))
    case _         => None
  }

  def takeByUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h,t), n) if n == 1 => Some((h(), (Stream.empty, n - 1)))
    case (Cons(h,t), n) if n > 0  => Some((h(), (t(), n - 1)))
    case _                        => None
  }

  def takeWhileByUnfold(f: A => Boolean): Stream[A] = unfold(this){
    case Cons(h,t) if f(h()) => Some((h(), t()))
    case _                   => None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h0,t0), Cons(h1,t1)) => Some((f(h0(), h1()), (t0(), t1())))
    case _                          => None
  }

  def zip[B](s2: Stream[B]): Stream[(A,B)] = 
      zipWith(s2)((_,_))


  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = 
      zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = 
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s)
    .takeWhile(!_._2.isEmpty)
    .forAll({case  (h0,h1) => h0 == h1})


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
