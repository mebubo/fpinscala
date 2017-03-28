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

}

import ch08.Gen

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }
}
