import RNG.Simple

object MainScala {

  def main(args: Array[String]): Unit = {

    println(List.sum(List(1,2,3,4,5)))

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    println(x)

    println("Hello! world");

    // RNG
    val rng1 = Simple(111)
    println(rng1.nextInt)
    /*
    val (n2 , rng2) = rng1.nextInt
    println(RNG.nonNegativeInt(rng2))
    println(RNG.double(rng2))

    val (n3 , rng3) = rng2.nextInt
    println(RNG.nonNegativeInt(rng3))
    println(RNG.double(rng3))

    val (n4 , rng4) = rng3.nextInt
    println(RNG.nonNegativeInt(rng4))
    println(RNG.double(rng4))

    */

    var (testN , rngTest) = rng1.nextInt
    for (i <-  1 until 50) {
      println(RNG.nonNegativeInt(rngTest))
      val (testN2 , rngTest2) = rngTest.nextInt
      rngTest = rngTest2
    }

  }
}
