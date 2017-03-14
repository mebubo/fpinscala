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
    (i / (i.toDouble + 1), rng2)
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
}
