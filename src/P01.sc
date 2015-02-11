object P01{
  def last(xs: List[Int]): Int = xs match {
    case Nil => throw new Exception("No elements")
    case head :: Nil => head
    case head :: xs => last(xs)
  }
  val l = List(1,2,3,4,5)
  last(l)
}