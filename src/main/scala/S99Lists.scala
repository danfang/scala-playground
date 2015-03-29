/**
 * A first look into Scala collections and functional programming
 * methodology.
 *
 * Problems from http://aperiodic.net/phil/scala/s-99/
 *
 * TODO: JUnit testing for these problems.
 *
 * @author Daniel Fang
 * @email danfang@uw.edu
 */
object S99Lists extends App {

  val testList = List(5, 10, 15)
  val palindromeList = List(1, 2, 3, 2, 1)
  val nestedList = List(List(1, 1), 2, List(3, List(5, 8)))
  val uncompressed = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val toRotate = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

  println("p01: " + lastN(testList, 1))
  println("p02: " + lastN(testList, 2))
  println("p03: " + testList(0))
  println("p04: " + length(testList))
  println("p05: " + reverse(testList))
  println("p06: " + isPalindrome(palindromeList))
  println("p07: " + flatten(nestedList))
  println("p08: " + compressDupes(uncompressed))
  println("p09: " + packDupes(uncompressed))
  println("p19: " + rotateLeft(3, toRotate))

  def lastN[A](lst : List[A], n : Int) = {
    if (n <= 0) throw new IllegalArgumentException
    if (lst.length < n) throw new NoSuchElementException
    lst.takeRight(n).head
  }

  def length[A](lst : List[A]) = {
    lst.foldLeft(0) { (count, el) => count + 1 };
  }

  def reverse[A](lst : List[A]) = {
    lst.foldLeft(List[A]()) { (partial, el) => el :: partial }
  }

  def isPalindrome[A](lst : List[A]) = lst == lst.reverse

  def flatten(lst : List[Any]) : List[Any] = {
    lst.flatMap({
      case nested : List[Any] => flatten(nested)
      case el => List(el)
    })
  }

  def compressDupes[A](lst : List[A]) : List[A] = {
    lst.foldRight(List[A]()) { (el, partial) =>
      if (partial.isEmpty || partial.head != el) el :: partial
      else partial
    }
  }

  def packDupes[A](lst : List[A]) : List[List[A]] = {
    val (packed, rest) = lst.span(el => el == lst.head)
    if (rest != Nil) packed :: packDupes(rest)
    else List(packed)
  }

  def rotateLeft[A](n : Int, lst : List[A]) : List[A] = {
    val r = n % lst.length
    if (r < 0) lst.takeRight(-r) ::: lst.dropRight(-r)
    else if (r > 0) lst.drop(r) ::: lst.take(r)
    else lst
  }
}