object P03{
  def kthElement(k: Int, xs: List[Int]): Int = k match {
    case x if x < 0             => throw new Exception("No negative index allowed")
    case x if xs.length < x + 1 => throw new Exception("Index out of bounds")
    case 0                      => xs.head
    case _ => kthElement(k-1,xs.tail)
  }
  val l = List(1,2,3,4,5)
  kthElement(2,l)
  kthElement(0,l)
}