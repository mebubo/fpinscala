package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeIntRetry(rng: RNG): (Int, RNG) = {
    var result = -1
    var nextRng = rng
    do {
      val p = rng.nextInt
      result = p._1
      nextRng = p._2
    } while (result < 0)
    (result, nextRng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    (if (i < 0) -(i + 1) else i, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    (i/(Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, n1) = rng.nextInt
    val (d, n2) = double(n1)
    ((i, d), n2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), nextRng) = intDouble(rng)
    ((d, i), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) {
      (List(), rng)
    } else {
      val (x, r1) = rng.nextInt
      val (xs, rn) = ints(count - 1)(r1)
      (x :: xs, rn)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count <= 0) (xs, r)
      else {
        val (x, r1) = r.nextInt
        go(count - 1, r1, x :: xs)
      }
    }
    go(count, rng, List())
  }

  val doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a, b), rb2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((curr, acc) => map2(curr, acc)(_ :: _))

  def sequenceLeft[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List[A]()))((acc, curr) => map2(curr, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r2) = f(rng)
      g(a)(r2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = n % i
      if (i + (n - 1) - mod >= 0)
        unit(i)
      else
        nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra){a => unit(f(a))}

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a => map(rb){b => f(a, b)}}
}

case class State[S,+A](run: S => (A, S)) {
  def map0[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = this.run(s)
      (f(a), s2)
    })

  def map[B](f: A => B): State[S, B] =
    this.flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s2) = this.run(s)
      f(a).run(s2)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(State.unit[S, List[A]](List())){(sa, sla) => sa.map2(sla)(_ :: _)}
}

object Main {
  def main(args: Array[String]): Unit = {
    println(RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(RNG.Simple(0))._1)
    println(RNG.sequenceLeft(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(RNG.Simple(0))._1)
  }
}
