object P02{
  def penultimate(xs: List[Int]): Int = xs match {
    case x :: y :: Nil => x
    case x :: y :: rest => penultimate(y :: rest)
    case _ => throw new Exception("No penultimate")
  }
  val l = List(1,2,3,4,5)
  val l2 = List(1)
  penultimate(l2)
}