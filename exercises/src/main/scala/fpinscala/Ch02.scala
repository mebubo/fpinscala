package fpinscala

object Ch02 {

  def fib(n: Int): Int = {
    def go(a: Int, b: Int, c: Int): Int =
      if (c == 0) a
      else go(b, a + b, c - 1)
    if (n < 0) -1
    else go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(arts: Array[String]) = {
    println(fib(-1))
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(100000000))

    val ordered: (Int, Int) => Boolean = (a: Int, b: Int) => a <= b
    val sortedResults = List(
      isSorted(Array(1, 2, 3), ordered),
      !isSorted(Array(3, 2, 3), ordered),
      !isSorted(Array(1, 2, 1), ordered),
      !isSorted(Array(1, -1, 3), ordered),
      isSorted(Array(1), ordered),
      isSorted(Array(), ordered),
      isSorted(Array(1, 1), ordered),
      !isSorted(Array(1, 0), ordered)
    )

    println(sortedResults.forall(b => b))

    def plus(a: Int, b: Int): Int = a + b
    val curriedPlus = curry(plus)
    val plusOne = curriedPlus(1)
    println(plusOne(1), plusOne(10), curriedPlus(3)(5), curry(plus)(2)(3))

    println(uncurry(curriedPlus)(1, 2))


    println(compose(math.abs, plusOne)(-10))
    println(compose(plusOne, math.abs)(-10))
  }
}
