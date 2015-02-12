object P15{
  def duplicateN(n: Int, ls: List[Any]): List[Any] = {
    ls match {
      case Nil => Nil
      case x :: rest => List.fill(n)(x) ++ duplicateN(n, rest)
    }
  }
  duplicateN(4, List(1,2,3))
}