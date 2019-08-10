import scala.annotation.tailrec

sealed trait  List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A , tail: List[A]) extends  List[A]

object List {

  @tailrec
  def sum(ints:List[Int] , acc:Int = 0):Int = ints match{
    case Nil => acc
    case Cons(x , xs) => sum(xs, x + acc)
  }

  def apply[A](as : A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head , apply(as.tail:_*))

}
