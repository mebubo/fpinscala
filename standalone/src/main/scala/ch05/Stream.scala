package ch05

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List()).reverse
  }

  def toList3: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => {
        buf += h()
        go(t())
      }
    }
    go(this)
  }

  def take(n: Integer): Stream[A] = {
    if (n <= 0)
      Empty
    else
      this match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => t().take(n-1))
      }
  }

  def take2(n: Integer): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Integer): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case x => x
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => {
      lazy val hd = h()
      if (p(hd))
        Stream.cons(hd, t().takeWhile(p))
      else
        Stream.empty
    }
    case x => x
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](b: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
    case _ => b
  }

  def exists2(p: A => Boolean): Boolean = this.foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A])((h, t) => {
    if (p(h))
      Stream.cons(h, t)
    else
      Stream.empty
  })

  def headOption2: Option[A] = this.foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    this.foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B])((h, t) => f(h) append t)

  def append[B >: A](t: => Stream[B]): Stream[B] =
    this.foldRight(t)((h, tt) => Stream.cons(h, tt))

  def map2[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def take3(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (n, Cons(h, t)) if n > 0 => Some((h(), (n - 1, t())))
    case _ => None
  }

  def takeWhile3(f: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((a, b) => (a, b))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile { case (_, s2) => !s2.isEmpty }.forAll { case (x, y) => x == y }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append Stream(Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    this.tails.exists(t => t.startsWith(s))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.foldRight((z, Stream(z)))((a, bs) => {
      lazy val bsl = bs
      val b = f(a, bsl._1)
      (b, Stream.cons(b, bsl._2))
    })._2

  def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] = cons(x, go(y, x+y))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def fibs2: Stream[Int] = unfold((0, 1)){case (a, b) => Some(a, (b, a + b))}

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def constant2[A](c: A): Stream[A] = unfold(c)(_ => Some(c, c))

  def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

}

object Main {
  def main(args: Array[String]) = {
    println(Stream.ones.take(3).toList)
    println(Stream.fibs.take(10).toList)
    println(Stream.fibs.take(10).map2(_ + 1).toList)
    println(Stream.fibs.take3(10).map2(_ + 1).toList)
    println(Stream.fibs.take3(5).zipAll(Stream.ones.take(2)).toList)
    println(Stream(1, 2, 3, 4).startsWith(Stream(1, 2)))
    println(Stream(1, 2, 3, 4).startsWith(Stream(0, 1, 2)))
    println(Stream(1, 2).startsWith(Stream(1, 2)))
    println(Stream(1).startsWith(Stream(1, 2)))
    println(Stream().startsWith(Stream()))
    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  }
}
