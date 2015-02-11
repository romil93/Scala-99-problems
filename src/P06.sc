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
