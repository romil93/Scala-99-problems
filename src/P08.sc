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