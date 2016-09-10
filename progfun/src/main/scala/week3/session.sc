object session {

  abstract class IntSet {
    def incl(x: Int): IntSet

    def contains(x: Int): Boolean

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def contains(x: Int) = false

    def incl(x: Int) = new NonEmpty(x, Empty, Empty)

    def union(other: IntSet) = other

    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int) =
      if (x == elem) true
      else if (x < elem) left.contains(x)
      else right.contains(x)

    def incl(x: Int) =
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this

    def union(other: IntSet) = left.union(right).union(other).incl(elem)

    override def toString = s"{ ${left} ${elem} ${right} }"
  }

  val tree1 = new NonEmpty(3, new NonEmpty(2, Empty, Empty), Empty)
  val tree2 = new NonEmpty(4, new NonEmpty(1, Empty, Empty), Empty)

  tree1.union(tree2)

  trait List[T] {
    def isEmpty: Boolean

    def head: T

    def tail: List[T]

    def nth(n: Int): T =
      if (n == 0) head
      else tail.nth(n - 1)

  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }

  class Nil[T] extends List[T] {
    def isEmpty = true

    def head = throw new NoSuchElementException("Nil.head")

    def tail = throw new NoSuchElementException("Nil.tail")

    def nth = throw new IndexOutOfBoundsException
  }

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  val list = singleton(1)

  list.nth(0)

}