object P05{
  def reverse(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: y => reverse(y) ++ List(x)
  }
  val l = List(1,2,3,4,5)
  reverse(l)
}