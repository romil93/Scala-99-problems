object P15{
  def duplicateN(n: Int, ls: List[Any]): List[Any] = {
    ls match {
      case Nil => Nil
      case x :: rest => duplicateN1(n, x) :: duplicateN(n, rest)
    }
  }
  def duplicateN1(k: Int, elem: Any): List[Any] = {
    k match {
      case 0 => Nil
      case x if(x>0) => elem :: duplicateN1(x-1, elem)
    }
  }
  duplicateN(2, List(1,2,3))
}