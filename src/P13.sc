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