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