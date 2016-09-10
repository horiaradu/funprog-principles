object session {
  def removeAt[T](n: Int, xs: List[T]): List[T] =
    (n, xs) match {
      case (_, List()) => throw new IndexOutOfBoundsException;
      case (0, h :: t) => t
      case (_, h :: t) => h :: removeAt(n - 1, t)
    }

  removeAt(1, List(1, 2, 3))

  removeAt(0, List(1, 2, 3))

  def flatten(xs: List[Any]): List[Any] =
    xs match {
      case Nil => Nil
      case h :: t => (h match {
        case hs: List[Any] => flatten(hs)
        case item => List(item)
      }) ::: flatten(t)
    }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))

  implicit def compareInts(x: Int, y: Int) = x - y

  //  implicit def compareIntsReverse(x: Int, y: Int) = y - x

  def mergeSort[T](xs: List[T])(implicit comparator: (T, T) => Int): List[T] = {
    def merge(fst: List[T], snd: List[T]): List[T] =
      (fst, snd) match {
        case (Nil, _) => snd
        case (_, Nil) => fst
        case (fstHead :: fstTail, sndHead :: sndTail) =>
          if (comparator(fstHead, sndHead) <= 0) fstHead :: merge(fstTail, snd)
          else sndHead :: merge(fst, sndTail)
      }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs.splitAt(n)
      merge(mergeSort(fst), mergeSort(snd))
    }
  }

  mergeSort(List(10, 1, 9, 2, 8, 3, 7, 4, 6, 5))

  //  def pack[T](xs: List[T]): List[List[T]] = {
  //    def packRec(xs: List[T], acc: List[List[T]]): List[List[T]] =
  //      (xs, acc) match {
  //        case (Nil, acc) => acc
  //        case (head :: tail, Nil) => packRec(tail, List(List(head)))
  //        case (head :: tail, (lastElem :: lastOccurrencesOfElem) :: rest) =>
  //          if (head == lastElem) packRec(tail, (head :: lastElem :: lastOccurrencesOfElem) :: rest)
  //          else packRec(tail, List(head) :: acc)
  //      }
  //
  //    packRec(xs, Nil).reverse
  //  }

  def pack[T](xs: List[T]): List[List[T]] =
    xs match {
      case Nil => Nil
      case head :: _ => {
        val (hs, rest) = xs.span(elem => elem == head)
        hs :: pack(rest)
      }
    }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map(elems => (elems.head, elems.length))

  pack(List(1, 2, 1, 1, 1, 2, 2, 3, 4, 4, 1))

  encode(List(1, 2, 1, 1, 1, 2, 2, 3, 4, 4, 1))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) ((x, acc) => f(x) :: acc)

  mapFun[Int, Int](List(1, 2, 3), x => x + 1)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((x, acc) => acc + 1)

  lengthFun(List(1, 2, 3))
}