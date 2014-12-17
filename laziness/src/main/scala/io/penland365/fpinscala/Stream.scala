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
