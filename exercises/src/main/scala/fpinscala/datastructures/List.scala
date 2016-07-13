package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => 1 + acc)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List [Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length3[A](xs: List[A]): Int = foldLeft(xs, 0)((acc, _) => acc + 1)

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((acc, curr) => Cons(curr, acc))

  def foldRightViaFoldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(xs, (b: B) => b)((accf, a) => (b: B) => accf(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
    foldRight(xs, (b: B) => b)((a, accf) => (b: B) => accf(f(b, a)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concat[A](xss: List[List[A]]): List[A] = foldRight(xss, Nil: List[A])(append)

  def addOneToEach(xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def eachToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String])((x, acc) => Cons(x.toString(), acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    foldRight(xs, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A](xs: List[A])(f: A => List[A]): List[A] = concat(map(xs)(f))

  def filterViaFlatMap[A](xs: List[A])(f: A => Boolean) = flatMap(xs)(x => if (f(x)) List(x) else Nil)

  def zipAdd(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(x, xs1) => ys match {
      case Nil => Nil
      case Cons(y, ys1) => Cons(x + y, zipAdd(xs1, ys1))
    }
  }

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs1), Cons(y, ys1)) => Cons(f(x, y), zipWith(xs1, ys1)(f))
  }

  def all(xs: List[Boolean]): Boolean = foldLeft(xs, true)(_ && _)

  def any(xs: List[Boolean]): Boolean = foldLeft(xs, false)(_ || _)

  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = a match {
    case Nil => b == Nil
    case Cons(h, t) => all(zipWith(a, b)(_ == _)) || hasSubsequence(t, b)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(List.x)
    println(List.reverse(List(1, 2, 3)))
    println(List.append2(List(1, 2), List(10, 20)))
    println(List.concat(List(List(1, 2), List(10, 20))))
    println(List.addOneToEach(List(1, 2, 3)))
    println(List.filterViaFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0))
    println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println(List.hasSubsequence(List(1, 2, 3, 4), List(4)))
    println(List.hasSubsequence(List(1, 2, 3, 4), List(5)))
    println(List.hasSubsequence(List(1, 2, 3, 4), List(1,3)))
  }
}
