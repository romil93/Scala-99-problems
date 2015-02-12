import org.scalatest.Matchers._
import org.scalatest.FlatSpec

/**
 * Created by indix on 12/2/15.
 */
class Scala99ProblemsTest extends FlatSpec {
  //Test for Problem 1 - finding the last element in a list
  "Scala99Problems" should "find last element in the List" in{
    Scala99Problems.last(List(1,2,3,4,5)) should be(5)

    val exception1 = intercept[IllegalArgumentException]{
      Scala99Problems.last(List())
    }
    exception1.getMessage should be("No elements")
  }



  "Scala99Problems" should "find the last but one element(penultimate element) in the list" in {
    //Test for Problem 2 - finding the penultimate element in the list
    Scala99Problems.penultimate(List(1, 2, 3, 4)) should be(3)

    var exception2 = intercept[IllegalArgumentException]{
      Scala99Problems.penultimate(List(1))
    }
    exception2.getMessage should be("No Penultimate")

    exception2 = intercept[IllegalArgumentException]{
      Scala99Problems.penultimate(List())
    }
    exception2.getMessage should be("No Penultimate")
  }


  "Scala99Problems" should "find the kth element in the list" in {
    //Test for Problem 3 - finding the kth element in the list
    Scala99Problems.kthElement(4, List(4,5,3,7,2,8,9,5)) should be(2)

    var exception3 = intercept[IndexOutOfBoundsException]{
      Scala99Problems.kthElement(-1,List(1,2,3,4))
    }
    exception3.getMessage should be("No negative index allowed")

    exception3 = intercept[IndexOutOfBoundsException]{
      Scala99Problems.kthElement(10,List(1,2,4,5))
    }
    exception3.getMessage should be("Index out of bounds")
  }


  "Scala99Problems" should "find the length of the List" in {
    //Test for Problem 4 - finding the length of the list
    Scala99Problems.length(List(1,2,3,4)) should be(4)
    Scala99Problems.length(List()) should be(0)
    Scala99Problems.length(Nil) should be(0)
  }


  "Scala99Problems" should "find the reverse of the List" in {
    //Test for Problems 5 - finding the reverse of the list
    Scala99Problems.reverse(List(1,2,3,4)) should be(List(4,3,2,1))
    Scala99Problems.reverse(Nil) should be(List())
    Scala99Problems.reverse(List()) should be(List())
  }


  "Scala99Problems" should "find whether the given List is a Palindrome or not" in {
    //Test for Problem 6 - find whether the List is a Palindrome or not
    Scala99Problems.isPalindrome(List(1,2,3,2,1)) should be(true)
    Scala99Problems.isPalindrome(List(1,1,2,2)) should be(false)
    Scala99Problems.isPalindrome(List()) should be(true)
    Scala99Problems.isPalindrome(Nil) should be(true)
    Scala99Problems.isPalindrome(List(1)) should be(true)

    Scala99Problems.isPalindromeAlternative(List(1,2,3,2,1)) should be(true)
    Scala99Problems.isPalindromeAlternative(List(1,1,2,2)) should be(false)
    Scala99Problems.isPalindromeAlternative(List()) should be(true)
    Scala99Problems.isPalindromeAlternative(Nil) should be(true)
    Scala99Problems.isPalindromeAlternative(List(1)) should be(true)
  }


  "Scala99Problems" should "find the flattened nested list structure" in {
    //Test for Problem 7 - find the flattened list of the nested list structure
    Scala99Problems.flatten(List(List(1, List(2, List(3))))) should be(List(1,2,3))
    Scala99Problems.flatten(List(1, 2, List(3, List(4, List())))) should be(List(1,2,3,4))
    Scala99Problems.flatten(List(List())) should be(List())
    Scala99Problems.flatten(Nil) should be(List())
  }


  "Scala99Problems" should "compress same consecutive elements in a nested list" in {
    //Test for Problem 8 - compress same consecutive elements into a nested list
    Scala99Problems.compress(List(1,1,1,2,2,2,3,3,3)) should be(List(1,2,3))
    Scala99Problems.compress(List(1,2,3,4,5)) should be(List(1,2,3,4,5))
    Scala99Problems.compress(List()) should be(List())
    Scala99Problems.compress(List(1,1,1,2,2,1,2,3,4,4,4)) should be(List(1,2,1,2,3,4))
  }
}
