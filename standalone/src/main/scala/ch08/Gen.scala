package ch08

import Prop._
import ch06.State
import ch06.RNG
import ch05.Stream

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll0[A](a: Gen[A])(p: A => Boolean): Prop = Prop((tcs, rng) => {
    val (passed, rng2) = Gen.listOfN(tcs, a).map(l => l.map(p)).map(allTrue).sample.run(rng)
    if (passed)
      Passed
    else
      Falsified("foo", 0)
  })

  def allTrue(l: List[Boolean]): Boolean =
    l.foldRight(true)((a, b) => a && b)

  def forAll[A](as: Gen[A])(p: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (p(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](a: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(a.sample.run(rng)))

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (n, rng) => {
      run(n, rng) match {
        case Passed => p.run(n, rng)
        case x => x
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (n, rng) => {
      run(n, rng) match {
        case Falsified(_, m) => p.run(n, rng)
        case x => x
      }
    }
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.map(RNG.nonNegativeLessThan(stopExclusive - start))(_ + start)))

  def unit[A](a: A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.map(RNG.nonNegativeInt)(_ % 2 == 0)))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => listOfN(n, g)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double.flatMap(d => if (d > g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)

}

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(f(_).forSize(n)))
}
