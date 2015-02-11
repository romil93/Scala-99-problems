object P12{
  def encode[T](ls: List[(Int, T)]) : List[T] = {
    ls flatMap { e => List.fill(e._1)(e._2)}
  }
  encode(List())
  encode(List((1,2),(3,4)))
  encode(List((2,3),(4,4),(4,1)))
}