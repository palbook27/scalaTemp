trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng:RNG) : (Int , RNG) = {
    val (i1 , r) = rng.nextInt
    (if (i1 < 0) -(i1 + 1) else i1, r)
  }

  def double(rng : RNG) : (Double, RNG) = {
    val (i1 , r) = rng.nextInt
    (i1 / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng:RNG) : ((Int , Double) , RNG) = {

  }

  def doubleInt(rng:RNG) : ((Double , Int) , RNG) = {

  }

  def double3(rng:RNG) : ((Double , Double, Double) , RNG) = {

  }

}