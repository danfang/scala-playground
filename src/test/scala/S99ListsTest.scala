import S99Lists._
import org.scalatest._

/**
 * Testing Suite for S99 List problems
 *
 * @author Daniel Fang <danfang@uw.edu>
 */
class S99ListsTest extends FlatSpec {

  /////////////////////////////////////////////////////////////////////////////////
  //// P01
  /////////////////////////////////////////////////////////////////////////////////

  "The last() method" should "return the last element in a list" in {
    assert(last(List(1)) == 1)
    assert(last(List(1, 2, 3)) == 3)
  }

  it should "throw NoSuchElementException if called on an empty list" in {
    intercept[NoSuchElementException] {
      last(List())
    }
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P02
  /////////////////////////////////////////////////////////////////////////////////

  "The penultimate() method" should "return the second to last element in a list" in {
    assert(penultimate(List(1, 2, 3)) == 2)
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P03
  /////////////////////////////////////////////////////////////////////////////////

  "The nth() method" should "return the element at the nth position in a list" in {
    val list = List(1, 2, 3)
    assert(nth(0, list) == 1)
    assert(nth(1, list) == 2)
    assert(nth(2, list) == 3)
  }

  it should "throw IllegalArgumentException if n is an undefined index in the list" in {
    intercept[IllegalArgumentException] {
      nth(3, List(1, 2, 3))
    }
    intercept[IllegalArgumentException] {
      nth(-1, List(1, 2, 3))
    }
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P04
  /////////////////////////////////////////////////////////////////////////////////

  "The length() method" should "find the number of elements in a list" in {
    assert(length(List(1, 2, 3)) == 3)
    assert(length(List()) == 0)
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P05
  /////////////////////////////////////////////////////////////////////////////////

  "The reverse() method" should "return a list in reverse order" in {
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  ///////////////////////////////////////////////////////////////////////////
  //// P06
  /////////////////////////////////////////////////////////////////////////////////

  "The isPalindrome() method" should "return true iff a list is palindromic" in {
    assert(isPalindrome(List(1, 2, 3, 2, 1)) == true)
    assert(isPalindrome(List(1, 2, 3, 4)) == false)
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P07
  /////////////////////////////////////////////////////////////////////////////////

  "The flatten() method" should
    "return a list with only the elements of a nested list structure" in {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  ///////////////////////////////////////////////////////////////////////////////
  //// P08
  /////////////////////////////////////////////////////////////////////////////////

  "The compress() method" should "return a list with all element duplications removed" in {
    assert(compress(
      List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'a, 'd)) ==
      List('a, 'b, 'c, 'a, 'd)
    )
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P09
  /////////////////////////////////////////////////////////////////////////////////

  "The pack() method" should "pack duplicate elements in a list into sublists" in {
    assert(pack(
      List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'a, 'd)) ==
      List(List('a, 'a, 'a), List('b, 'b), List('c), List('a, 'a), List('d))
    )
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P10
  /////////////////////////////////////////////////////////////////////////////////

  "The encode() method" should "encode duplicate elements in a list as tuples " +
    "(N, E) where N is the number of duplicates of E" in {
    assert(encode(
      List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'a, 'd)) ==
      List((3, 'a), (2, 'b), (1, 'c), (2, 'a), (1, 'd))
    )
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P11
  /////////////////////////////////////////////////////////////////////////////////

  "The encodeModified() method" should "follow encode(), but leave non-duplicate elements as such" in {
    assert(encodeModified(
      List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'a, 'd)) ==
      List((3, 'a), (2, 'b), 'c, (2, 'a), 'd)
    )
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P12
  /////////////////////////////////////////////////////////////////////////////////

  "The decode() method" should "return an encode()'d list in its original format" in {
    assert(decode(
      List((3, 'a), (2, 'b), (1, 'c), (2, 'a), (1, 'd))) ==
      List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'a, 'd)
    )
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P13
  /////////////////////////////////////////////////////////////////////////////////

  "The encodeDirect() method" should "behave according to encode()" in {
    assert(encodeDirect(
      List('a, 'a, 'a, 'b, 'b, 'c, 'a, 'a, 'd)) ==
      List((3, 'a), (2, 'b), (1, 'c), (2, 'a), (1, 'd))
    )
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P14
  /////////////////////////////////////////////////////////////////////////////////

  "The duplicate() method" should "return a list with each element of an existing list duplicated" in {
    assert(duplicate(List(1, 2, 3)) == List(1, 1, 2, 2, 3, 3))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P15
  /////////////////////////////////////////////////////////////////////////////////

  "The duplicateN() method" should "duplicate elements of a list N times" in {
    assert(duplicateN(3, List(1, 2)) == List(1, 1, 1, 2, 2, 2))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P16
  /////////////////////////////////////////////////////////////////////////////////

  "The drop() method" should "drop every nth element from a list" in {
    assert(drop(
      3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    )
  }

  it should "not alter lists with fewer than n elements" in {
    assert(drop(3, List(1, 2)) == List(1, 2))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P17
  /////////////////////////////////////////////////////////////////////////////////

  "The split() method" should "split a list into two lists at an index, n, " +
    "and return them as a tuple" in {
    assert(split(2, List(1, 2, 3, 4, 5)) ==(List(1, 2), List(3, 4, 5)))
    assert(split(0, List(1, 2)) ==(List(), List(1, 2)))
    assert(split(2, List(1, 2)) ==(List(1, 2), List()))
  }

  it should "throw IllegalArgumentException for an index outside of the list" in {
    intercept[IllegalArgumentException] {
      split(-1, List())
    }

    intercept[IllegalArgumentException] {
      split(3, List(1, 2))
    }
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P18
  /////////////////////////////////////////////////////////////////////////////////

  "The slice() method" should "take a slice of a list[i:k] given indices i and k" in {
    assert(slice(
      3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
      List('d, 'e, 'f, 'g)
    )
    assert(slice(0, 3, List(1, 2, 3)) == List(1, 2, 3))
    assert(slice(3, 3, List(1, 2, 3)) == List())
  }

  it should "throw IllegalArgumentException for an invalid slice subsection" in {
    intercept[IllegalArgumentException] {
      slice(-1, 0, List())
    }
    intercept[IllegalArgumentException] {
      slice(0, -1, List())
    }
    intercept[IllegalArgumentException] {
      slice(2, 1, List(1, 2, 3))
    }
    intercept[IllegalArgumentException] {
      slice(0, 4, List(1, 2, 3))
    }
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P19
  /////////////////////////////////////////////////////////////////////////////////

  "The rotate() method" should "rotate a list N places to the left" in {
    assert(rotate(3, List('a, 'b, 'c, 'd)) == List('d, 'a, 'b, 'c))
    assert(rotate(2, List('a, 'b)) == List('a, 'b))
    assert(rotate(-1, List('a, 'b, 'c)) == List('c, 'a, 'b))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P20
  /////////////////////////////////////////////////////////////////////////////////

  "The removeAt() method" should "return a tuple with the kth element removed paired with that element" in {
    assert(removeAt(2, List(1, 2, 3)) ==(List(1, 2), 3))
    assert(removeAt(3, List('a, 'b, 'c, 'd, 'e, 'f)) ==(List('a, 'b, 'c, 'e, 'f), 'd))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P20
  /////////////////////////////////////////////////////////////////////////////////

  "The insertAt() method" should "return a list with a new element, el, inserted at index k" in {
    assert(insertAt(0, 0, List(1, 2, 3)) == List(0, 1, 2, 3))
    assert(insertAt('n, 2, List('a, 'b, 'c, 'd, 'e, 'f)) == List('a, 'b, 'n, 'c, 'd, 'e, 'f))
    assert(insertAt(4, 3, List(1, 2, 3)) == List(1, 2, 3, 4))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P21
  /////////////////////////////////////////////////////////////////////////////////

  "The range() method" should "create a list of integers between a given start and end integer" in {
    assert(range(1, 2) == List(1, 2))
    assert(range(-1, 1) == List(-1, 0, 1))
    assert(range(3, 0) == List(3, 2, 1, 0))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P23
  /////////////////////////////////////////////////////////////////////////////////

  "The randomSelect() method" should "generate a list of n elements taken randomly from a list." in {
    val random = randomSelect(2, List(1, 2))
    assert(random == List(1, 2) || random == List(2, 1))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P24
  /////////////////////////////////////////////////////////////////////////////////

  "The lotto() method" should "draw N distinct random numbers from a range 1 to M" in {
    val random = lotto(10, 50)
    assert(random.length == 10 && random.forall(a => List.range(1, 51).contains(a)))
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P25
  /////////////////////////////////////////////////////////////////////////////////

  "The randomPermute() method" should "create a list that is a random permutation of another list" in {
    assert(randomPermute(List(1, 2, 3)).length == 3)
  }

  /////////////////////////////////////////////////////////////////////////////////
  //// P26
  /////////////////////////////////////////////////////////////////////////////////

  "The combinations() method" should "generate all possible groupings of K members " +
    "given N elements in a list" in {
    assert(combinations(3, List.range(0, 12)).length == 220)
  }
}