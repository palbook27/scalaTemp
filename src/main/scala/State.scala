
case class State[S , +A](run: S => (A , S)) {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // def flatMap[A , B](f: Rand[A])(g: A => Rand[B]): Rand[B]
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  //def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
  //  List.foldRight(sas , unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)


object State {

}
