import shapeless._

class d {
  trait S
  trait V
  trait T[A]
  trait C[A, B]

  implicit def a0[A, B](implicit
                        a: Lazy[T[A]],
                        b: T[B]): T[C[A, B]] = ???

  implicit def a1(
      implicit
      a: T[C[V, C[V, V]]]): T[S] = ???

  implicit val a2: T[V] = ???

  implicitly[T[C[S, V]]]
}
