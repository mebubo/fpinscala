package ch07

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A) = fork(unit(a))

  def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }

  def fork[A](pa: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = pa(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
}
