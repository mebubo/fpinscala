package ch09

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]
  def many1[A](p: Parser[A]): Parser[List[A]]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)]
  def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    val numA: Parser[Int] = char('a').many.map(_.size)
  }
}
