package ch04

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: Option[B]): Option[B] = this.map(x => Some(x)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if (f(x)) Some(x) else None)
}

case object None extends Option[Nothing]
case class Some[A](get: A) extends Option[A]

object Main {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a_ => b.map(b_ => f(a_, b_)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(List()))((a, b) => map2(a, b)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(List()))((a, b) => map2(f(a), b)(_ :: _))

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(x => x)

  def main(args: Array[String]) = {
    println(mean(List(1,2,3,4)).toString)
    println(variance(List(1,2,3,4)).toString)
  }
}
