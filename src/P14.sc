object P14{
  def duplicate[T](ls: List[T]): List[T] = {
    if(ls.isEmpty) Nil
    else
      ls.head ::  ls.head :: duplicate(ls.tail)
  }
  duplicate(List('a,'b,'c,'d,'e))
}