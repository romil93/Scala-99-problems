object P04{
  def length(xs: List[Int]): Int = xs match {
    case Nil => 0
    case _ => 1 + length(xs.tail)
  }
  val l = List(1,2,3,4)
  length(l)
  length(List())
}