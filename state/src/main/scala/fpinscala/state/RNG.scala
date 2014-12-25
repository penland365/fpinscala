package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val i = rng.nextInt
    val j = if(i._1 == Int.MinValue) 0 else i._1.abs
    (j, i._2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val(i, nextRng) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1.0), nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val i = nonNegativeInt(rng)
    val j = double(i._2)
    ((i._1, j._1), j._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val i = double(rng)
    val j = nonNegativeInt(i._2)
    ((i._1, j._1), j._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val i = double(rng)
    val j = double(i._2)
    val k = double(j._2)
    ((i._1, j._1, k._1), k._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if(n > 0) {
        val(i, nextRng) = nonNegativeInt(r)
        go(n - 1, i :: l, nextRng)
      }
      else (l, r)
    }
    go(count, Nil, rng)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
