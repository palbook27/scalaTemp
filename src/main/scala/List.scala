import scala.annotation.tailrec

sealed trait  List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A , tail: List[A]) extends  List[A]

object List {

  @tailrec
  final def sum(ints:List[Int] , acc:Int = 0):Int = ints match{
    case Nil => acc
    case Cons(x , xs) => sum(xs, x + acc)
  }

  final def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  final def apply[A](as : A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head , apply(as.tail:_*))

}
