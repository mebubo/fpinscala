package ch06

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i,  rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (acc, r)
      } else {
        val (i, rng2) = rng.nextInt
        go(c - 1, rng2, i :: acc)
      }
    }
    go(count, rng, List())
  }

  type Rand[A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = r => (a, r)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    r => {
      val (a, r2) = s(r)
      (f(a), r2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDouble2: Rand[(Int, Double)] = both(int, double)

  def doubleInt2: Rand[(Double, Int)] = both(double, int)

  def sequence[A](xs: List[Rand[A]]): Rand[List[A]] =
    xs.foldRight(unit(List[A]()))((x, r) => map2(x, r)(_ :: _))

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = ra(rng)
      val rb = f(a)
      rb(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(int)(i => {
      val max = (Int.MaxValue / n) * n
      if (i > max) nonNegativeLessThan(n)
      else unit(i % n)
    })

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap (a => sb map (b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _  ))

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {

  def step(i: Input, m: Machine): Machine =
    (i, m) match {
      case (Coin, Machine(true, ca, co)) if ca > 0 => Machine(false, ca, co + 1)
      case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)
      case _ => m
    }

  def step2(i: Input): State[Machine, Unit] = for {
    s <- State.get
    _ <- State.set(step(i, s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val is: List[State[Machine, Unit]] = inputs.map(step2)
    for {
      _ <- State.sequence(is)
      s <- State.get
    } yield (s.coins, s.candies)
  }
}

object Main {
  def main(args: Array[String]) = {
    println(RNG.nonNegativeLessThan(6)(SimpleRNG(1)).toString)
    println(CandyMachine.simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(true, 10, 0)).toString)
  }
}

