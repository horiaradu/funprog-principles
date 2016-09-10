object session {

  abstract class Nat {
    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat = new Succ(this)

    def +(that: Nat): Nat

    def -(that: Nat): Nat
  }

  object Zero extends Nat {

    def isZero = true

    def predecessor = throw new Error("0.predecessor")

    def +(that: Nat) = that

    def -(that: Nat) =
      if (that.isZero) this
      else throw new Error("0.-")

    override def toString = "0"
  }

  class Succ(n: Nat) extends Nat {
    def isZero = false

    def predecessor = n

    def +(that: Nat) = new Succ(n + that)

    def -(that: Nat) =
      if (that.isZero) this
      else n - that.predecessor

    override def toString = s"Succ ( ${n} )"
  }

  val x = new Succ(new Succ(Zero))

  val y = new Succ(Zero)

  x + y

  x - y


  /**
    * Lists
    */

  trait List[+T] {
    def isEmpty: Boolean

    def head: T

    def tail: List[T]

    def nth(n: Int): T =
      if (n == 0) head
      else tail.nth(n - 1)

    def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false

    override def toString = s"[ ${head},  ${tail} ]"
  }

  object Nil extends List[Nothing] {
    def isEmpty = true

    def head = throw new NoSuchElementException("Nil.head")

    def tail = throw new NoSuchElementException("Nil.tail")

    def nth = throw new IndexOutOfBoundsException

    override def toString = "[]"
  }

  object List {
    def apply[T]() = Nil

    def apply[T](a: T) = new Cons(a, Nil)

    def apply[T](a: T, b: T) = new Cons(a, new Cons(b, Nil))
  }

  val l = List(1, 2)

  val empty: List[String] = Nil

  class A {}

  class B extends A {}

  class C extends A {}

  def f(xs: List[B], x: A) = xs.prepend(x)

  def g(xs: List[C], x: B) = xs.prepend(x)

}