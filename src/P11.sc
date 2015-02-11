object P11{
  def pack[T](xs: List[T]): List[List[T]] = {
    if (xs.isEmpty) List(List())
    else {
      val (packed, next) = xs span {_ == xs.head} // returns two lists - it spans the list xs till the condition is matched and returns the rest.
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
  def encoding[T](list: List[T]): List[(Int, T)] = {
    pack(list) map { x => (x.length, x.head)}
  }
  def encodingModified(xs: List[Any]): List[Any] = {
    pack(xs) map { x => if(x.length == 1) (x.head) else (x.length, x.head) }
  }
  val l = List(1,1,1,1,1,2,2,2,2,3,4,5,5,1,1)
  encodingModified(l)
}