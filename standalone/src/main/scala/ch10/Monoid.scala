package ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 1
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero = 0
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): (A => A) = a1 compose a2
    def zero = x => x
  }

  import ch08.Prop
  import ch08.Gen

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)){ case (x, y, z) => m.op(m.op(x, y), z) == m.op(x, m.op(y, z)) }
    Prop.forAll(gen)((a: A) => m.op(a, m.zero) == a)
    Prop.forAll(gen)((a: A) => m.op(m.zero, a) == a)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldMap2[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 0)
      m.zero
    else if (v.length == 1)
      f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  import ch07.Par
  import ch07.Par._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op(a: Par[A], b: Par[A]): Par[A] = Par.map2(a, b)(m.op)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.parMap(v.toList)(f)) { bs =>
      foldMapV(bs.toIndexedSeq, par(m))(b => Par.lazyUnit(b))
    }

  import ch04.Option
  import ch04.Option._

  def ordered(v: IndexedSeq[Int]): Boolean = {
    val orderedMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
      val zero = None
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        Option.map2(a1, a2) { case ((min1, max1, b1), (min2, max2, b2)) => (min1 min min2, max1 max max2, b1 && b2 && max1 <= min2) }
    }
    foldMapV(v, orderedMonoid)(x => Some(x, x, true)).map(_._3).getOrElse(false)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    val zero = Stub("")

    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(l, w, r)) => Part(s1 + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    def unstub(x: String): Int = if (x.isEmpty) 0 else 1
    foldMapV(s.toIndexedSeq, wcMonoid){ c =>
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
      } match {
        case Stub(x) => unstub(x)
        case Part(l, w, r) => unstub(l) + w + unstub(r)
      }

  }

}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid)(z)
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid))(z)
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldRight(as)(m.zero)((a, b) => m.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
    Monoid.foldMapV(as, m)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(r)(z)(f))(f)
  }
  override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](a: Option[A])(z: B)(f: (A, B) => B): B = a match {
    case None => z
    case Some(x) => f(x, z)
  }
  override def foldLeft[A, B](a: Option[A])(z: B)(f: (B, A) => B): B = a match {
    case None => z
    case Some(x) => f(z, x)
  }
  override def foldMap[A, B](a: Option[A])(f: A => B)(m: Monoid[B]): B = a match {
    case None => m.zero
    case Some(x) => f(x)
  }
}
