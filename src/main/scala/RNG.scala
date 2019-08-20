
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
    val (i1 , r1) = rng.nextInt
    val (d1 , r2) = double(r1)
    ((i1 , d1) , r2)
  }

  def doubleInt(rng:RNG) : ((Double , Int) , RNG) = {
    val ((i1, d1) , r) = intDouble(rng)
    ((d1, i1) , r)
  }

  def double3(rng:RNG) : ((Double , Double, Double) , RNG) = {
    val (d1 , r1) = double(rng)
    val (d2 , r2) = double(r1)
    val (d3 , r3) = double(r2)
    ((d1, d2, d3) , r3)
  }
  /*
  def ints(count: Int)(rng: RNG): (List[Int] , RNG) = {
    if (count < 1)
      (List() , rng)
    else {
      val (i1 , r1) = rng.nextInt
      val (is2 , r2) = ints(count - 1)(r1)
      (i1 :: is2 , r2)
    }
  }
*/
  type Rand[+A] = RNG => (A , RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A , B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i%2)

  def double2: Rand[Double] =
    map(nonNegativeInt)(_  / (Int.MaxValue.toDouble + 1))

  def map2[A , B , C](ra: Rand[A] , rb: Rand[B])(f:(A , B) => C): Rand[C] = {
    rng => {
      val (a2 , rngA) = ra(rng)
      val (b2 , rngB) = rb(rngA)
      (f(a2,b2) , rngB)
    }
  }

  def both[A , B](ra:Rand[A] , rb:Rand[B]): Rand[(A , B)] =
    map2(ra , rb)((_ , _))

  val randIntDouble: Rand[(Int , Double)] =
    both(int , double)

  val randDoubleInt: Rand[(Double , Int)] =
    both(double , int)

  // List[Rand[A]] = (RNG => (A , RNG) , RNG => (A , RNG) , RNG => (A , RNG) ...)
  // Rand[List[A]] = RNG => (List[A] , RNG)
  // def foldRight[A , B](as: List[A], z: B)(f: (A, B) => B): B
  //def sequence[A](fs: List[Rand[A]]) : Rand[List[A]] = {
  //  List.foldRight(fs , unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  // }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (i , rng2) = nonNegativeInt(rng)
      val mod = i % n
      if(i + (n-1) - mod >= 0)
        (mod , rng2)
      else nonNegativeLessThan(n)(rng2)
  }

  def flatMap[A , B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a , r1) = f(rng)
      g(a)(r1)
    }
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def _map[A , B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A , B , C](ra: Rand[A] , rb: Rand[B])(f:(A , B) => C): Rand[C] =
    flatMap(ra)(a => _map(rb)(b => f(a,b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

