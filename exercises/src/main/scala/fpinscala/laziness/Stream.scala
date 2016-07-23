package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, Nil).reverse
  }

  def toListTailRecViaBuffer: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B>:A](other: => Stream[B]): Stream[B] =
    foldRight(other)((h, t) => cons(h, t))

  def flatMap[B>:A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), m) if m > 0 => Some(h(), (t(), m-1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean = zipWithAll(this, s)((opta, optb) => (opta, optb) match {
    case (Some(a), Some(b)) => a == b
    case (Some(_), None) => true
    case _ => false
  }).forAll(x => x)

  def startsWith2[B](s: Stream[B]): Boolean = zipAll(this, s)
    .takeWhile(_._2.isDefined)
    .forAll {
      case (a, b) => a == b
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(s: S): Stream[A] = {
      f(s).map({case (a, s2) => cons(a, go(s2))}).getOrElse(empty)
    }
    go(z)
  }

  val onesViaUnfold: Stream[Int] = unfold(1)(s => Some((s, s)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(0)(_ => Some((a, 0)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))

  val fibsViaUnfold: Stream[Int] = unfold((0, 1))({ case (a, b) => Some(a, (b, a+b)) })


  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((as, bs)) {
      case (Cons(a, ta), Cons(b, tb)) => Some(f(a(), b()), (ta(), tb()))
      case _ => None
    }

  def zipWithAll[A, B, C](as: Stream[A], bs: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((as, bs)) {
      case (Cons(a, ta), Cons(b, tb)) => Some(f(Some(a()), Some(b())), (ta(), tb()))
      case (Cons(a, ta), Empty) => Some(f(Some(a()), Option.empty[B]), (ta(), empty))
      case (Empty, Cons(b, tb)) => Some(f(Option.empty[A], Some(b())), (empty, tb()))
      case _ => None
    }

  def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(as, bs)((_, _))

}

object Main {
  def main(args: Array[String]) {
    println(Stream(1, 2).headOption)
    println(Stream(1).headOption)
    println(empty.headOption)
    val stream = Stream(1, 2, 3, 4, 5)
    println(stream.takeWhile(_ < 4).toList)
    println(stream.filter(_ % 2 == 0).toList)

    println(ones.take(5).toList)
    println(onesViaUnfold.take(5).toList)
    println(constantViaUnfold(2).take(5).toList)
    println(fromViaUnfold(2).take(5).toList)
    println(fibs.take(20).toList)
    println(fibsViaUnfold.take(20).toList)

    println(Stream(1, 2, 3).takeViaUnfold(1).toList)
    println(Stream(1, 2, 3).takeViaUnfold(4).toList)
    println(Stream(1, 2, 3).take(4).toList)
    println(Stream(1).takeViaUnfold(4).toList)
    println(Stream().takeViaUnfold(4).toList)
    println(Stream(1, 2).takeViaUnfold(0).toList)
    println(Stream().takeViaUnfold(0).toList)

    println(Stream(1,2,3).startsWith2(Stream(1,2,3)))
    println(Stream(1,2,3).startsWith2(Stream(1,2)))
    println(Stream(1,2,3).startsWith2(Stream(1)))
    println(Stream(1,2,3).startsWith2(Stream()))

    println(Stream(1,2).startsWith2(Stream(1, 2, 3)))
    println(Stream(1).startsWith2(Stream(1, 2, 3)))
    println(Stream().startsWith2(Stream(1, 2, 3)))

    println(Stream(1).startsWith2(Stream(1)))
    println(Stream().startsWith2(Stream()))

  }
}
