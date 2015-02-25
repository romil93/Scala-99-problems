class Scala_99 {

}

object Scala99Problems{
  def last(xs: List[Int]): Int = xs match {
    case Nil => throw new IllegalArgumentException("No elements")
    case head :: Nil => head
    case head :: xs => last(xs)
  }

  def penultimate(xs: List[Int]): Int = xs match {
    case x :: y :: Nil => x
    case x :: y :: rest => penultimate(y :: rest)
    case _ => throw new IllegalArgumentException("No Penultimate")
  }

  def kthElement(k: Int, xs: List[Int]): Int = k match {
    case x if x < 0             => throw new IndexOutOfBoundsException("No negative index allowed")
    case x if xs.length < x + 1 => throw new IndexOutOfBoundsException("Index out of bounds")
    case 0                      => xs.head
    case _ => kthElement(k-1,xs.tail)
  }

  def length(xs: List[Int]): Int = xs match {
    case Nil => 0
    case _ => 1 + length(xs.tail)
  }

  def reverse(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: y => reverse(y) ++ List(x)
  }

  def isPalindrome[T](xs: List[T]): Boolean ={
    xs match {
      case Nil => true
      case head :: Nil => true
      case _ => (xs.head == xs.last) && isPalindrome(xs.tail.reverse.tail.reverse)
    }
  }


  def isPalindromeAlternative[T](xs: List[T]): Boolean = {
    xs == xs.reverse
  }

  def flatten(list: List[Any]): List[Any] = {
    def flatten1(remaining: List[Any], output: List[Any]): List[Any] =
      remaining match {
        case Nil => output
        case (x: List[_]) :: y => flatten1(y, output ::: flatten(x))
        case x :: y => flatten1(y, output :+ x)
      }
    flatten1(list, Nil)
  }

  def compress(list: List[Any]): List[Any] = {
    list match {
      case Nil => Nil
      case x :: Nil => list
      case x :: y :: z => if (x == y) compress(y :: z) else List(x) ::: compress(y :: z)
    }
  }

  def pack[T](ls: List[T]): List[List[T]] = {
    if (ls.isEmpty) List(List())
    else {
      val (same, rest) = ls  span { _ == ls.head }// returns two lists - it spans the list ls till the condition is matched and returns the rest.
      if (rest == Nil) List(same)
      else same :: pack(rest)
    }
  }

  def encoding[T](list: List[T]): List[(Int, T)] = {
    pack(list) map { x => (x.length, x.head)}
  }

  def decode[T](ls: List[(Int, T)]) : List[T] = {
    ls flatMap { e => List.fill(e._1)(e._2)}
  }

  def encodeDirect[T](ls: List[T]): List[(Int, T)] = {
    if(ls.isEmpty){
      Nil
    }
    else{
      val (same, rest) = ls  span { _ == ls.head }
      (same.length, ls.head) :: encodeDirect(rest)
    }
  }

  def duplicate[T](ls: List[T]): List[T] = {
    if(ls.isEmpty) Nil
    else
      ls.head ::  ls.head :: duplicate(ls.tail)
  }

  def duplicateN(n: Int, ls: List[Any]): List[Any] = {
    ls match {
      case Nil => Nil
      case x :: rest => List.fill(n)(x) ++ duplicateN(n, rest)
    }
  }

  def split(n: Int, ls: List[Any], l1: List[Any] = List(), l2: List[Any] = List()): (List[Any], List[Any]) = {

    def recursiveSplit() = {
      n match {
        case 0 => split(0, Nil, l1, l2 ++ ls)
        case num if num > 0 => split(num - 1, ls.tail, l1 ++ List(ls.head), l2)
      }
    }

    ls match {
      case Nil => (l1, l2)
      case _ => recursiveSplit()
    }
  }

  def slice(ls: List[Any], i: Int, k: Int): List[Any] = {

    def listLength(ls: List[Any]): Int = {
      ls.length
    }

    def isListEmpty(ls: List[Any]): Boolean = {
      ls.isEmpty
    }

    def sliceActual(ls: List[Any], k: Int, ans: List[Any] = Nil): List[Any] = {
      if (isListEmpty(ls)) throw new IllegalArgumentException("The list is too small for the slice")

      k match {
        case 0 => ans
        case num => sliceActual(ls.tail, k-1, ans ++ List(ls.head))
      }
    }

    if (isListEmpty(ls)){
      throw new IllegalArgumentException("Empty List cannot be sliced")
    }

    else if (listLength(ls) < i || i > k) throw new IllegalArgumentException("Invalid i and k arguments")

    else{
      i match {
        case 1 => sliceActual(ls, k)
        case num => slice(ls.tail, i - 1, k - 1)
      }
    }
  }
}

object P01{
  def last(xs: List[Int]): Int = xs match {
    case Nil => throw new IllegalArgumentException("No elements")
    case head :: Nil => head
    case head :: xs => last(xs)
  }
  val l = List(1,2,3,4,5)
  last(l)
}

object P02{
  def penultimate(xs: List[Int]): Int = xs match {
    case x :: y :: Nil => x
    case x :: y :: rest => penultimate(y :: rest)
    case _ => throw new Exception("No penultimate")
  }
  val l = List(1,2,3,4,5)
  val l2 = List(1)
  penultimate(l2)
}

object P03{
  def kthElement(k: Int, xs: List[Int]): Int = k match {
    case x if x < 0             => throw new Exception("No negative index allowed")
    case x if xs.length < x + 1 => throw new Exception("Index out of bounds")
    case 0                      => xs.head
    case _ => kthElement(k-1,xs.tail)
  }
  val l = List(1,2,3,4,5)
  kthElement(2,l)
  kthElement(0,l)
}

object P04{
  def length(xs: List[Int]): Int = xs match {
    case Nil => 0
    case _ => 1 + length(xs.tail)
  }
  val l = List(1,2,3,4)
  length(l)
  length(List())
}

object P05{
  def reverse(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: y => reverse(y) ++ List(x)
  }
  val l = List(1,2,3,4,5)
  reverse(l)
}

object P06{
  def isPalindrome[T](xs: List[T]): Boolean ={
    xs match {
      case Nil => true
      case head :: Nil => true
      case _ => (xs.head == xs.last) && isPalindrome(xs.tail.reverse.tail.reverse)
    }
  }


  def isPalindromeAlternative[T](xs: List[T]): Boolean = {
    xs == xs.reverse
  }
  isPalindrome(List(2, 2, 3, 2, 1))
  isPalindrome(List(2, 2, 3, 2, 2))
  isPalindrome(List(2))
  isPalindrome(List(2, 3))
  isPalindromeAlternative(List(2, 2))
  isPalindromeAlternative(List())
}


object P07{
  val l = List(List(1,1,1),List(2,List(3,List(2,1)),2),4,5)

  def flatten(list: List[Any]): List[Any] = {
    def flatten1(remaining: List[Any], output: List[Any]): List[Any] =
      remaining match {
        case Nil => output
        case (x: List[_]) :: y => flatten1(y, output ::: flatten(x))
        case x :: y => flatten1(y, output :+ x)
      }
    flatten1(list, Nil)
  }

  flatten(l)
}

//x is a general reference to the head and y to the tail

object P08{
  def compress(list: List[Any]): List[Any] = {
    list match {
      case Nil => Nil
      case x :: Nil => list
      case x :: y :: z => if (x == y) compress(y :: z) else List(x) ::: compress(y :: z)
    }
  }
  compress(List(1))
  compress(List())
  compress(List(1,1,1,1,1,1))
  compress(List(1,2,3,4))
  compress(List(1, 1, 1, 1, 2, 3, 4, 4, 4, 4, 4, 1))
}

object P09{
  def pack[T](ls: List[T]): List[List[T]] = {
    if (ls.isEmpty) List(List())
    else {
      val (same, rest) = ls  span { _ == ls.head }// returns two lists - it spans the list ls till the condition is matched and returns the rest.
      if (rest == Nil) List(same)
      else same :: pack(rest)
    }
  }
  val l = List(1,1,1,1,2,2,3,4,1,1,1,1)
  pack(l)
}

object P10{
  def pack[T](ls: List[T]): List[List[T]] = {
    if (ls.isEmpty) List(List())
    else {
      val (same, rest) = ls span {_ == ls.head} // returns two lists - it spans the list ls till the condition is matched and returns the rest.
      if (rest == Nil) List(same)
      else same :: pack(rest)
    }
  }
  def encoding[T](list: List[T]): List[(Int, T)] = {
    pack(list) map { x => (x.length, x.head)}
  }

  val l =List(1,1,1,1,2,2,2,3,3,4)
  encoding(l)
}

object P11{
  def pack[T](ls: List[T]): List[List[T]] = {
    if (ls.isEmpty) List(List())
    else {
      val (same, rest) = ls span {_ == ls.head} // returns two lists - it spans the list ls till the condition is matched and returns the rest.
      if (rest == Nil) List(same)
      else same :: pack(rest)
    }
  }
  def encoding[T](list: List[T]): List[(Int, T)] = {
    pack(list) map { x => (x.length, x.head)}
  }

  val l =List(1,1,1,1,2,2,2,3,3,4)
  encoding(l)
}

object P12{
  def decode[T](ls: List[(Int, T)]) : List[T] = {
    ls flatMap { e => List.fill(e._1)(e._2)}
  }
  decode(List())
  decode(List((1,2),(3,4)))
  decode(List((2,3),(4,4),(4,1)))
}

object P13{
  def encodeDirect[T](ls: List[T]): List[(Int, T)] = {
    if(ls.isEmpty){
      Nil
    }
    else{
      val (same, rest) = ls  span { _ == ls.head }
      (same.length, ls.head) :: encodeDirect(rest)
    }
  }
  encodeDirect(List(1,1,1,1,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5))
  encodeDirect(List())
}

object P14{
  def duplicate[T](ls: List[T]): List[T] = {
    if(ls.isEmpty) Nil
    else
      ls.head ::  ls.head :: duplicate(ls.tail)
  }
  duplicate(List('a,'b,'c,'d,'e))
}

object P15{
  def duplicateN(n: Int, ls: List[Any]): List[Any] = {
    ls match {
      case Nil => Nil
      case x :: rest => List.fill(n)(x) ++ duplicateN(n, rest)
    }
  }
  duplicateN(4, List(1,2,3))
}

object P17{
  def split(n: Int, ls: List[Any], l1: List[Any] = List(), l2: List[Any] = List()): (List[Any], List[Any]) = {

    def recursiveSplit() = {
      n match {
        case 0 => split(0, Nil, l1, l2 ++ ls)
        case num if num > 0 => split(num - 1, ls.tail, l1 ++ List(ls.head), l2)
      }
    }

    ls match {
      case Nil => (l1, l2)
      case _ => recursiveSplit()
    }
  }
}

object P18{
  def slice(ls: List[Any], i: Int, k: Int): List[Any] = {

    def listLength(ls: List[Any]): Int = {
      ls.length
    }

    def isListEmpty(ls: List[Any]): Boolean = {
      ls.isEmpty
    }

    def sliceActual(ls: List[Any], k: Int, ans: List[Any] = Nil): List[Any] = {
      if (isListEmpty(ls)) throw new IllegalArgumentException("The list is too small for the slice")

      k match {
        case 0 => ans
        case num => sliceActual(ls.tail, k-1, ans ++ List(ls.head))
      }
    }

    if (isListEmpty(ls)){
      throw new IllegalArgumentException("Empty List cannot be sliced")
    }

    else if (listLength(ls) < i || i > k) throw new IllegalArgumentException("Invalid i and k arguments")

    else{
      i match {
        case 1 => sliceActual(ls, k)
        case num => slice(ls.tail, i - 1, k - 1)
      }
    }
  }
}