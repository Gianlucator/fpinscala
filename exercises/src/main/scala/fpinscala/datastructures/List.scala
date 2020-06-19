package fpinscala.datastructures


import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(head, tail) => tail;
    case Nil => Nil
  }
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(head, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Cons(_, tail) => drop(tail, n-1)
      case Nil => drop(Nil, 0)
    }
  }

  // L'inferenza del tipo di input della funzione f non funziona in questo modo, va specificato
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
    case Nil => Nil
  }

  // f è in grado di inferire automaticamente il tipo
  def dropWhileTypeInferred[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) => if (f(h)) dropWhileTypeInferred(t)(f) else Cons(h, t)
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => Cons(h, init(t))
    case Cons(t, Nil) => Nil
    case Nil => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((l, length) => length + 1)

  // Stack safe
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z,h))(f)
  }


  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))


  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b: B, a: A) => f(a, b))
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Int]) =
    foldLeft(ns, 1)(_*_)

  def length2[A](ns: List[A]): Int = ns match {
    case Nil => 0
    case Cons(h, t) => foldLeft(t, 1)((x, _) => x + 1)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def mapToSucc(l: List[Int]) = {
    map(l)(_ + 1)
  }

  def mapDoubleToString(l: List[Int]): List[String] = map(l)(_.toString)

  def filter[A](l: List[A])(pred: A => Boolean): List[A] = l match {
    case Nil        => Nil
    case Cons(h, t) => if (pred(h)) Cons(h, t) else filter(t)
  }
  def append[A](l: List[A], b: List[A]): List[A] = {
    foldRight(l, Nil:List[A])(Cons(_, _))
  }
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil:List[B]
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def main(args: Array[String]): Unit = {
    val a = List(1,2,3,4,5)
    /*
    println(List.tail(a))
    println(List.setHead(a, 4))
    println(List.drop(a, 2))
    println(List.dropWhile(a, (x: Int) => x < 4) )
    println(List.dropWhileTypeInferred(a)(_ < 4))
    println(List.init(a))
    println(List.sum3(a))
    println(length2(a))

     */
    // println(foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)))
    println(foldRightViaFoldLeft(a, 0)(_+_))
  }
}
