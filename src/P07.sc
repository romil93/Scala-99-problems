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