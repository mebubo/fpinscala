package ch11

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fa) => map(fa)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lfa: List[F[A]]): F[List[A]] =
    lfa.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_ :: _))

  def traverse[A, B](lfa: List[A])(f: A => F[B]): F[List[B]] =
    lfa.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def filterM0[A](la: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val lfab: List[F[(A, Boolean)]] = la.map(a => map(f(a))(b => (a, b)))
    map(sequence(lfab))(l => l.filter(_._2).map(_._1))
  }

  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] =
    la.foldRight(unit(List[A]()))((a, fla) => {
      val fb = f(a)
      map2(fb, fla)((b, la) => if (b) a :: la else la)
    })

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)
}

import ch08.Gen
import ch07.Par
import ch07.Par._
import ch09.Parsers

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: A): Par[A] = Par.unit(a)
    def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: A): List[A] = List[A]()
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: A): Stream[A] = Stream(a)
    def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
      fa.flatMap(f)
  }

  // def parserMonad[P[_]](p: Parsers[String, P]) = new Monad[P] {
  //   def unit[A](a: A): P[A] = p.succeed(a)
  //   def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] =
  //     p.flatMap(fa)(f)
  // }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = Id(a)
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = new Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => {
        f(fa.run(r)).run(r)
      })
  }
}

object Main {
  def main(args: Array[String]) =
    println(Monad.optionMonad.filterM(List(1, 2, 3))(x => Some(x % 2 == 0)))
    println(Monad.optionMonad.filterM0(List(1, 2, 3))(x => Some(x % 2 == 0)))
}
