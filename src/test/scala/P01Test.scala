import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class P01Test extends FlatSpec {

  "P01" should "find the last element in the list" in {
    P01.last(List(1,2,3,4,5)) should be(5)
    val exception = intercept[IllegalArgumentException] {
      P01.last(Nil)
    }
    exception.getMessage should be("No elements")
  }
}
