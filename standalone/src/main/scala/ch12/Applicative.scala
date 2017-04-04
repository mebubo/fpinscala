package ch12

import ch11.Functor
import ch11.Monad

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab)((a, ab) => ab(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(x => x)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: A): (F[A], G[A]) = (Applicative.this.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (Applicative.this.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: A): F[G[A]] = Applicative.this.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        Applicative.this.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: A): Validation[E, A] = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
      case (e@Failure(_, _), _) => e
      case (_, e@Failure(_, _)) => e
    }

  }
}

object Monad2 {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }
}
