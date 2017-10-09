def flatten(xs: List[Any]): List[Any] = {
  xs match {
    case head :: tail => (
      head match {
        case list: List[Any] => flatten(list)
        case _ => List(head)
      }) ::: flatten(tail)
    case _ => xs
  }
}

List(List(1, 1), 2, List(3, List(5, 8)))
flatten(List(List(1, 1), 2, List(3, List(5, 8))))


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) ((x: T, y: List[U]) => f(x) :: y)


mapFun(List(1, 2, 3), (x: Int) => x * x)


def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0) ((x: T, y: Int) => y + 1)

lengthFun(List(1, 2, 3))