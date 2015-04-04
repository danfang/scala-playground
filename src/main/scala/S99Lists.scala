import scala.util.Random

/**
 * A first look into Scala collections and functional programming
 * methodology. This collection of methods deals with Scala lists.
 * Note that many problems can be done with built-ins - the goal is to
 * implement most tasks functionally.
 *
 * Problems from http://aperiodic.net/phil/scala/s-99/
 *
 * @author Daniel Fang <danfang@uw.edu>
 */
object S99Lists {

  // Returns the last element within a list
  def last[A](lst: List[A]) = lastN(1, lst)

  // Returns the second to last element in a list
  def penultimate[A](lst: List[A]) = lastN(2, lst)

  // Takes a list and returns the nth to last element
  def lastN[A](n: Int, lst: List[A]) = {
    if (n <= 0) throw new IllegalArgumentException
    if (lst.length < n) throw new NoSuchElementException
    lst.takeRight(n).head
  }

  def nth[A](n: Int, lst: List[A]) = {
    if (n >= lst.length || n < 0) throw new IllegalArgumentException
    lst(n)
  }

  // Functionally calculates the length of a list
  def length[A](lst: List[A]) = {
    lst.foldLeft(0) { (count, el) => count + 1 };
  }

  // Functionally reverses a list
  def reverse[A](lst: List[A]) = {
    lst.foldLeft(List[A]()) { (partial, el) => el :: partial }
    // built-in: lst.reverse
  }

  // Returns whether or not a list is a palindrome
  def isPalindrome[A](lst: List[A]) = lst == lst.reverse

  // Takes a nested list structure and returns just the elements as a list
  def flatten(lst: List[Any]): List[Any] = {
    lst.flatMap({
      case nested: List[Any] => flatten(nested)
      case el => List(el)
    })
  }

  // Removes consecutive duplicates from a list
  def compress[A](lst: List[A]): List[A] = {
    lst.foldRight(List[A]()) { (el, partial) =>
      if (partial.isEmpty || partial.head != el) el :: partial
      else partial
    }
  }

  // Takes consecutive duplicates and puts them into lists,
  // returning a List of Lists. e.g. List(1, 1, 2, 3, 3, 3) ->
  // List(List(1, 1), List(2), List(3, 3, 3))
  def pack[A](lst: List[A]): List[List[A]] = {
    val (packed, rest) = lst.span(el => el == lst.head)
    if (rest != Nil) packed :: pack(rest)
    else List(packed)
  }

  // Consecutive duplicates of elements are encoded as tuples (N, E)
  // where N is the number of duplicates of the element E.
  def encode[A](lst: List[A]): List[(Int, A)] = {
    pack(lst).foldRight(List[(Int, A)]()) { (packed, partial) =>
      (packed.length, packed(0)) :: partial
    }
  }

  // Consecutive duplicates of elements are encoded as tuples (N, E)
  // where N is the number of duplicates of the element E.
  def encodeModified[A](lst: List[A]): List[Any] = {
    pack(lst).foldRight(List[Any]()) { (packed, partial) =>
      if (packed.length > 1)
        (packed.length, packed(0)) :: partial
      else
        packed(0) :: partial
    }
  }

  // Decodes an encoded List as per encode()
  def decode[A](lst: List[(Int, A)]): List[A] = {
    lst.flatMap(tuple => List.fill(tuple._1)(tuple._2))
  }

  // Directly encodes a list without packing the values first
  def encodeDirect[A](lst: List[A]): List[(Int, A)] = {
    val (packed, rest) = lst.span(el => el == lst.head)
    if (rest != Nil) (packed.length, packed(0)) :: encodeDirect(rest)
    else List((packed.length, packed(0)))
  }

  // Duplicate each element within a list
  def duplicate[A](lst: List[A]): List[A] = duplicateN(2, lst)

  // Duplicate each element within a list N times
  def duplicateN[A](n: Int, lst: List[A]): List[A] = {
    lst.flatMap(el => List.fill(n)(el))
  }

  // Drop each nth element in a list
  def drop[A](n: Int, lst: List[A]): List[A] = {
    lst.grouped(n).flatMap(grouped => {
      if (grouped.length == n) grouped.init
      else grouped
    }).toList
  }

  // Split a list into two parts, given an index n
  // built-in: lst.splitAt(n)
  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    if (n > lst.length || n < 0) throw new IllegalArgumentException
    (lst.take(n), lst.drop(n))
  }

  // Slice a list from indices [i, k)
  def slice[A](i: Int, k: Int, lst: List[A]): List[A] = {
    if (i > k || i < 0 || k < 0 || k > lst.length || i > lst.length)
      throw new IllegalArgumentException
    lst.drop(i).dropRight(lst.length - k)
  }

  // Rotates a list n times from the left, given any integer n.
  def rotate[A](n: Int, lst: List[A]): List[A] = {
    val r = n % lst.length
    if (r < 0) lst.takeRight(-r) ::: lst.dropRight(-r)
    else if (r > 0) lst.drop(r) ::: lst.take(r)
    else lst
  }

  // Remove the kth element of a list and return a tuple with
  // the new list paired with the removed element.
  def removeAt[A](k: Int, lst: List[A]): ((List[A]), A) = {
    (lst.take(k) ::: lst.takeRight(lst.length - k - 1), lst(k))
  }

  // Insert an element into the kth index of a list.
  def insertAt[A](el: A, k: Int, lst: List[A]): List[A] = {
    (lst.take(k) :+ el) ::: lst.takeRight(lst.length - k)
  }

  // Create a list with integers between a given range.
  def range(a: Int, b: Int): List[Int] = {
    if (a < b) a :: range(a + 1, b)
    else if (a > b) a :: range(a - 1, b)
    else List(b)
  }

  // Create a list of length n of randomly selected items from a list.
  def randomSelect[A](n: Int, lst: List[A]): List[A] = {
    if (n > lst.length)
      throw new IllegalArgumentException("there must be at least n items in lst")
    if (n > 0) {
      val tuple = removeAt(Random.nextInt(lst.length), lst)
      tuple._2 :: randomSelect(n - 1, tuple._1)
    } else Nil
  }

  // Draw N different random numbers from the set 1..M
  def lotto(n: Int, m: Int): List[Int] = {
    randomSelect(n, List.range(1, m + 1))
  }

  // Generate a random permutation of elements within a list
  def randomPermute[A](lst: List[A]): List[A] = {
    randomSelect(lst.length, lst)
  }

  // Generate the combinations of K distinct objects chosen from the N elements of a list.
  // Note lst.combinations(k) is the built-in to do this
  def combinations[A](k: Int, lst: List[A]): List[List[A]] = {
    if (k <= 0) throw new IllegalArgumentException
    if (lst.length == 0 || k > lst.length) Nil
    // Use foldLeft to build a List[List] of permutations as well as keep track
    // of the current index of the iteration
    lst.foldLeft((List[List[A]](), 0)) {
      (tuple, el) => {
        val (res, i) = tuple
        // If n == 1, the permutations are just all the individual elements
        if (k == 1) (res :+ List(el), i + 1)
        else {
          // Otherwise, permutations are the removed element, el,
          // prepended to the permutations of the rest of the list.
          (res ::: combinations(k - 1, lst.drop(i+1)).map(a => el :: a), i + 1)
        }
      }
    }._1
  }

  // Same as combinations, but ordering counts as distinct combinations,
  // e.g (2, 1) and (1, 2) are two different combinations.
  def combinationsWithOrder[A](k: Int, lst: List[A]): List[List[A]] = {
    if (k <= 0 || k > lst.length) throw new IllegalArgumentException
    lst.foldLeft((List[List[A]](), 0)) {
      (tuple, el) => {
        val (res, i) = tuple
        if (k == 1)(res :+ List(el), i + 1)
        else {
          val (rest, el) = removeAt(i, lst)
          (res ::: combinationsWithOrder(k - 1, rest).map(a => el :: a), i + 1)
        }
      }
    }._1
  }

  // Sort the elements of a list according to their length
  def lsort(lst: List[Seq[Any]]): List[Seq[Any]] =
    lst.sortBy(_.length)

  // Sort the elements according to their length frequency, e.g.
  // least common list lengths first
  def lsortFreq(lst: List[Seq[Any]]): List[Seq[Any]] = {
    lst.groupBy(_.length)       // Group list by element length
      .values.groupBy(_.length) // Group groups by group length (freq)
      .toSeq.sortBy(_._1)       // Sort groups by frequency (key)
      .flatMap(_._2.flatten)    // Transform into Iterable[List]
      .toList
  }
}