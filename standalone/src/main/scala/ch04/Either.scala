package ch04

sealed trait Either[+E, +A]
case class Left[E](value: E) extends Either[E, Nothing]
case class Right[A](value: A) extends Either[Nothing, A]

object Main2 {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
}
