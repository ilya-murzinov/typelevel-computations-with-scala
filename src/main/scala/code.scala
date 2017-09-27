import shapeless.test._
import shapeless.Lazy

object q extends App {
  trait List
  trait Nil extends List
  trait Cons[H, T <: List] extends List
  type ::[H, T <: List] = Cons[H, T]

  trait Nat
  trait Z extends Nat
  trait Succ[Z <: Nat] extends Nat

  type _0 = Z
  type _1 = Succ[Z]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]

  trait List1 {
    type Concat1[That <: List1] <: List1
  }
  trait Nil1 extends List1 {
    type Concat1[That <: List1] = That
  }
  trait Cons1[H, T <: List1] extends List1 {
    type First = H
    type Concat1[That <: List1] = Cons1[H, T#Concat1[That]]
  }
  implicitly[Cons1[_1, Nil1]#First =:= _1]
  implicitly[Nil1#Concat1[Nil1] =:= Nil1]

  trait First[L <: List] { type Out }
  object First {
    def apply[L <: List](implicit f: First[L]): Aux[L, f.Out] = f
    type Aux[L <: List, H] = First[L] { type Out = H }
    implicit def f[H, T <: List]: Aux[Cons[H, T], H] = ???
  }

  trait Concat[A <: List, B <: List] { type Out <: List }
  object Concat {
    type Aux[A <: List, B <: List, C <: List] = Concat[A, B] { type Out = C }
    implicit def c0[A <: List]: Aux[Nil, A, A] = ???
    implicit def c1[A, B <: List, C <: List](
        implicit c: Concat[B, C]
    ): Aux[Cons[A, B], C, Cons[A, c.Out]] = ???
  }

  trait ConcatAll[Ls <: List] { type Out <: List }
  object ConcatAll {
    def apply[Ls <: List](implicit ca: ConcatAll[Ls]): Aux[Ls, ca.Out] = ca
    type Aux[Ls <: List, L <: List] = ConcatAll[Ls] { type Out = L }
    implicit val c0: Aux[Nil, Nil] = ???
    implicit def c1[Chunk <: List, Rest <: List, Acc <: List](
        implicit
        ca: Aux[Rest, Acc],
        c: Concat[Chunk, Acc]): Aux[Cons[Chunk, Rest], c.Out] = ???
  }

  trait Bool
  class True extends Bool
  class False extends Bool

  trait AnyTrue[L] { type Out <: Bool }
  object AnyTrue {
    type Aux[L <: List, B <: Bool] = AnyTrue[L] { type Out = B }
    implicit val a0: Aux[Nil, False] = ???
    implicit def a1[T <: List]: Aux[True :: T, True] = ???
    implicit def a2[T <: List](
        implicit
        a: AnyTrue[T]): Aux[False :: T, a.Out] = ???
  }

  trait Not[A <: Bool] { type Out <: Bool }
  object Not {
    type Aux[A <: Bool, B <: Bool] = Not[A] { type Out = B }
    implicit val not0: Aux[True, False] = ???
    implicit val not1: Aux[False, True] = ???
  }

  trait Or[A <: Bool, B <: Bool] { type Out <: Bool }
  object Or {
    type Aux[A <: Bool, B <: Bool, C <: Bool] = Or[A, B] { type Out = C }
    implicit val or0: Aux[True, True, True] = ???
    implicit val or1: Aux[True, False, True] = ???
    implicit val or2: Aux[False, True, True] = ???
    implicit val or3: Aux[False, False, False] = ???
  }

  trait Eq[A <: Nat, B <: Nat] { type Out <: Bool }
  object Eq {
    type Aux[A <: Nat, B <: Nat, C <: Bool] = Eq[A, B] { type Out = C }

    implicit val eq0: Aux[_0, _0, True] = ???
    implicit def eq1[A <: Nat]: Aux[_0, Succ[A], False] = ???
    implicit def eq2[A <: Nat]: Aux[Succ[A], _0, False] = ???
    implicit def eq3[A <: Nat, B <: Nat](
        implicit e: Eq[A, B]): Aux[Succ[A], Succ[B], e.Out] = ???
  }

  trait Lt[A <: Nat, B <: Nat] { type Out <: Bool }
  object Lt {
    type Aux[A <: Nat, B <: Nat, C <: Bool] = Lt[A, B] { type Out = C }
    implicit val lt0: Aux[_0, _0, False] = ???
    implicit def lt1[A <: Nat]: Aux[_0, Succ[A], True] = ???
    implicit def lt2[A <: Nat]: Aux[Succ[A], _0, False] = ???
    implicit def lt3[A <: Nat, B <: Nat](
        implicit l: Lt[A, B]): Aux[Succ[A], Succ[B], l.Out] = ???
  }

  trait AbsDiff[A <: Nat, B <: Nat] { type Out <: Nat }
  object AbsDiff {
    type Aux[A <: Nat, B <: Nat, C <: Nat] = AbsDiff[A, B] { type Out = C }
    implicit val a0: Aux[_0, _0, _0] = ???
    implicit def a1[A <: Nat]: Aux[A, _0, A] = ???
    implicit def a2[A <: Nat]: Aux[_0, A, A] = ???
    implicit def a3[A <: Nat, B <: Nat](
        implicit a: AbsDiff[A, B]): Aux[Succ[A], Succ[B], a.Out] = ???
  }

  trait Range[A <: Nat] { type Out <: List }
  object Range {
    type Aux[A <: Nat, L <: List] = Range[A] { type Out = L }
    implicit val r0: Aux[_0, Nil] = ???
    implicit def r1[A <: Nat](implicit r: Range[A]): Aux[Succ[A], A :: r.Out] =
      ???
  }

  trait Func
  trait Conj[L <: List] extends Func

  trait Apply[F <: Func, A] { type Out }
  object Apply {
    def apply[F <: Func, A](implicit a: Apply[F, A]): Aux[F, A, a.Out] = a
    type Aux[F <: Func, A, R] = Apply[F, A] { type Out = R }
    implicit def a0[L <: List, A]: Aux[Conj[L], A, A :: L] = ???
  }

  trait Map[F <: Func, L <: List] { type Out <: List }
  object Map {
    def apply[F <: Func, L <: List](implicit m: Map[F, L]): Aux[F, L, m.Out] = m
    type Aux[F <: Func, L <: List, R <: List] = Map[F, L] { type Out = R }
    implicit def m0[F <: Func]: Aux[F, Nil, Nil] = ???
    implicit def m1[F <: Func, Head, Tail <: List, R](
        implicit
        a: Apply[F, Head],
        m: Map[F, Tail]): Aux[F, Head :: Tail, a.Out :: m.Out] = ???
  }

  trait MapCat[F <: Func, L <: List] { type Out <: List }
  object MapCat {
    def apply[F <: Func, L <: List](
        implicit mc: MapCat[F, L]): Aux[F, L, mc.Out] = mc
    type Aux[F <: Func, L <: List, R <: List] = MapCat[F, L] { type Out = R }
    implicit def mc0[F <: Func]: Aux[F, Nil, Nil] = ???
    implicit def mc1[F <: Func, X, Xs <: List, Chunks <: List](
        implicit
        m: Map.Aux[F, X :: Xs, Chunks],
        ca: ConcatAll[Chunks]): Aux[F, X :: Xs, ca.Out] = ???
  }

  trait AppendIf[B <: Bool, A, L <: List] { type Out <: List }
  object AppendIf {
    type Aux[B <: Bool, A, L <: List, R <: List] = AppendIf[B, A, L] {
      type Out = R
    }
    implicit def ai0[A, L <: List]: Aux[True, A, L, A :: L] = ???
    implicit def ai1[A, L <: List]: Aux[False, A, L, L] = ???
  }

  trait Filter[F <: Func, L <: List] { type Out <: List }
  object Filter {
    def apply[F <: Func, L <: List](
        implicit f: Filter[F, L]): Aux[F, L, f.Out] = f
    type Aux[F <: Func, L <: List, R <: List] = Filter[F, L] { type Out = R }
    implicit def f0[F <: Func]: Aux[F, Nil, Nil] = ???
    implicit def f1[F <: Func, H, T <: List, B <: Bool, FT <: List](
        implicit
        a: Apply.Aux[F, H, B],
        f: Filter.Aux[F, T, FT],
        ai: AppendIf[B, H, FT]): Aux[F, Cons[H, T], ai.Out] = ???
  }

  class Queen[X <: Nat, Y <: Nat]
  class QueenX[X <: Nat] extends Func
  object QueenX {
    implicit def qa[X <: Nat, Y <: Nat]: Apply.Aux[QueenX[X], Y, Queen[X, Y]] =
      ???
  }

  trait QueensInRow[Y <: Nat, N <: Nat] { type Out <: List }
  object QueensInRow {
    def apply[Y <: Nat, N <: Nat](
        implicit q: QueensInRow[Y, N]): Aux[Y, N, q.Out] = q
    type Aux[Y <: Nat, N <: Nat, R <: List] = QueensInRow[Y, N] { type Out = R }
    implicit def qir0[Y <: Nat, N <: Nat, R <: List](
        implicit
        r: Range.Aux[N, R],
        m: Map[QueenX[Y], R]): Aux[Y, N, m.Out] = ???
  }

  trait Threatens[Q1 <: Queen[_, _], Q2 <: Queen[_, _]] { type Out <: Bool }
  object Threatens {
    type Aux[Q1 <: Queen[_, _], Q2 <: Queen[_, _], R <: Bool] =
      Threatens[Q1, Q2] { type Out = R }

    implicit def t0[X1 <: Nat,
                    Y1 <: Nat,
                    X2 <: Nat,
                    Y2 <: Nat,
                    EqX <: Bool,
                    EqY <: Bool,
                    EqXY <: Bool,
                    DX <: Nat,
                    DY <: Nat,
                    EqD <: Bool](
        implicit
        eqX: Eq.Aux[X1, X2, EqX],
        eqY: Eq.Aux[Y1, Y2, EqY],
        or0: Or.Aux[EqX, EqY, EqXY],
        dx: AbsDiff.Aux[X1, X2, DX],
        dy: AbsDiff.Aux[Y1, Y2, DY],
        eqD: Eq.Aux[DX, DY, EqD],
        res: Or[EqXY, EqD]): Aux[Queen[X1, Y1], Queen[X2, Y2], res.Out] = ???
  }

  trait Threatens1[Q <: Queen[_, _]] extends Func
  object Threatens1 {
    implicit def t0[Q1 <: Queen[_, _], Q2 <: Queen[_, _]](
        implicit t: Threatens[Q1, Q2]): Apply.Aux[Threatens1[Q1], Q2, t.Out] =
      ???
  }

  trait Safe[Config <: List, Q <: Queen[_, _]] { type Out <: Bool }
  object Safe {
    def apply[Config <: List, Q <: Queen[_, _]](
        implicit s: Safe[Config, Q]): Aux[Config, Q, s.Out] = s
    type Aux[Config <: List, Q <: Queen[_, _], R] = Safe[Config, Q] {
      type Out = R
    }
    implicit def s0[Config <: List, Q <: Queen[_, _], L <: List, T1 <: Bool](
        implicit
        m: Map.Aux[Threatens1[Q], Config, L],
        t1: AnyTrue.Aux[L, T1],
        t2: Not[T1]): Aux[Config, Q, t2.Out] = ???
  }

  trait Safe1[Config <: List] extends Func
  object Safe1 {
    implicit def a[Config <: List, Q <: Queen[_, _]](
        implicit
        s: Safe[Config, Q]): Apply.Aux[Safe1[Config], Q, s.Out] = ???
  }

  trait AddQueen[N <: Nat, X <: Nat, Config <: List] { type Out <: List }
  object AddQueen {
    def apply[N <: Nat, X <: Nat, Config <: List](
        implicit a: AddQueen[N, X, Config]): Aux[N, X, Config, a.Out] = a

    type Aux[N <: Nat, X <: Nat, Config <: List, R <: List] =
      AddQueen[N, X, Config] { type Out = R }

    implicit def aq0[N <: Nat, X <: Nat, Config <: List, Qs <: List, S <: List](
        implicit
        qr: QueensInRow.Aux[X, N, Qs],
        f: Filter.Aux[Safe1[Config], Qs, S],
        m: Map[Conj[Config], S]): Aux[N, X, Config, m.Out] = ???
  }

  trait AddQueen2[N <: Nat, X <: Nat] extends Func
  object AddQueen2 {
    implicit def a0[N <: Nat, X <: Nat, Config <: List](
        implicit
        a: AddQueen[N, X, Config]): Apply.Aux[AddQueen2[N, X], Config, a.Out] =
      ???
  }

  trait AddQueenToAll[N <: Nat, X <: Nat, Configs <: List] { type Out <: List }
  object AddQueenToAll {
    def apply[N <: Nat, X <: Nat, Configs <: List](
        implicit a: AddQueenToAll[N, X, Configs]): Aux[N, X, Configs, a.Out] = a
    type Aux[N <: Nat, X <: Nat, Configs <: List, R <: List] =
      AddQueenToAll[N, X, Configs] { type Out = R }
    implicit def a0[N <: Nat, X <: Nat, Configs <: List](
        implicit
        m: MapCat[AddQueen2[N, X], Configs]): Aux[N, X, Configs, m.Out] = ???
  }

  trait AddQueensIf[P <: Bool, N <: Nat, X <: Nat, Configs <: List] {
    type Out <: List
  }
  object AddQueensIf {
    def apply[P <: Bool, N <: Nat, X <: Nat, Configs <: List](
        implicit
        a: AddQueensIf[P, N, X, Configs]): Aux[P, N, X, Configs, a.Out] = a

    type Aux[P <: Bool, N <: Nat, X <: Nat, Configs <: List, R <: List] =
      AddQueensIf[P, N, X, Configs] { type Out = R }

    implicit def a0[N <: Nat, X <: Nat, Configs <: List]
      : Aux[False, N, X, Configs, Configs] = ???
    implicit def a1[N <: Nat,
                    X <: Nat,
                    Configs <: List,
                    Configs2 <: List,
                    R <: List](implicit
                               aqa: AddQueenToAll.Aux[N, X, Configs, Configs2],
                               aq: AddQueens.Aux[N, Succ[X], Configs2, R])
      : Aux[True, N, X, Configs, R] = ???
  }

  trait AddQueens[N <: Nat, X <: Nat, Configs <: List] { type Out <: List }
  object AddQueens {
    def apply[N <: Nat, X <: Nat, Configs <: List](
        implicit a: AddQueens[N, X, Configs]): Aux[N, X, Configs, a.Out] = a
    type Aux[N <: Nat, X <: Nat, Configs <: List, R <: List] =
      AddQueens[N, X, Configs] { type Out = R }
    implicit def a0[N <: Nat, X <: Nat, Configs <: List, LT <: Bool, R <: List](
        implicit
        lt: Lt.Aux[X, N, LT],
        aq: AddQueensIf.Aux[LT, N, X, Configs, R])
      : Aux[N, X, Configs, R] = ???
  }

  trait Solution[N <: Nat] { type Out <: List }
  object Solution {
    def apply[N <: Nat](implicit s: Solution[N]): Aux[N, s.Out] = s
    type Aux[N <: Nat, R <: List] = Solution[N] { type Out = R }
    implicit def s0[N <: Nat, Configs <: List](
        implicit
        aq: AddQueens.Aux[N, _0, Nil :: Nil, Configs],
        f: First[Configs]): Aux[N, Nil] = ???
  }

  println(showType(AddQueensIf[True, _4, _1, (Queen[_0, _0] :: Nil) :: Nil]))
  //println(showType(AddQueens[_4, _1, (Queen[_0, _0] :: Nil) :: Nil]))
  //println(showType(Solution[_4]))

  trait Specs {
    // implicitly[First.Aux[_1 :: Nil, _1]]
    // illTyped("implicitly[First[Nil]]")

    // implicitly[Concat.Aux[_1 :: Nil, Nil, _1 :: Nil]]
    // implicitly[Concat.Aux[_1 :: _2 :: Nil,
    //                       _3 :: _4 :: Nil,
    //                       (_1 :: _2 :: _3 :: _4 :: Nil)]]
    // illTyped("implicitly[Concat.Aux[_1 :: Nil, Nil, Nil]]")

    // private type A = _1 :: _2 :: Nil
    // private type B = _3 :: _4 :: Nil
    // private type L = A :: B :: Nil
    // private type C = _1 :: _2 :: _3 :: _4 :: Nil
    // implicitly[ConcatAll.Aux[L, C]]

    // implicitly[AnyTrue.Aux[True :: Nil, True]]
    // implicitly[AnyTrue.Aux[False :: True :: Nil, True]]
    // implicitly[AnyTrue.Aux[False :: False :: Nil, False]]

    // implicitly[Or.Aux[True, False, True]]
    // implicitly[Or.Aux[True, False, True]]

    // implicitly[Eq.Aux[_1, _1, True]]
    // implicitly[Eq.Aux[_1, _2, False]]

    // implicitly[Lt.Aux[_2, _2, False]]
    // implicitly[Lt.Aux[_4, _5, True]]
    // implicitly[Lt.Aux[_6, _5, False]]

    // implicitly[AbsDiff.Aux[_3, _5, _2]]
    // implicitly[AbsDiff.Aux[_1, _6, _5]]
    // illTyped("implicitly[AbsDiff.Aux[_1, _6, _2]]")

    // implicitly[Range.Aux[_0, Nil]]
    // implicitly[Range.Aux[_3, _2 :: _1 :: _0 :: Nil]]

    // implicitly[Map.Aux[Conj[_1 :: _2 :: Nil], Nil, Nil]]
    // implicitly[
    //   Map.Aux[Conj[Nil], _1 :: _2 :: Nil, (_1 :: Nil) :: (_2 :: Nil) :: Nil]]

    // implicitly[MapCat.Aux[Conj[_1 :: _2 :: Nil], Nil, Nil]]
    // implicitly[
    //   MapCat.Aux[Conj[_2 :: Nil], _1 :: _1 :: Nil, _1 :: _2 :: _1 :: _2 :: Nil]]

    // trait SomeF extends Func
    // trait FilterSpec extends FilterSpec0 {
    //   implicit val sf1: Apply.Aux[SomeF, _2, False] = ???
    //   implicitly[Filter.Aux[SomeF, _1 :: _2 :: _3 :: Nil, _1 :: _3 :: Nil]]
    // }
    // trait FilterSpec0 {
    //   implicit def sf0[A]: Apply.Aux[SomeF, A, True] = ???
    // }

    // implicitly[Apply.Aux[QueenX[_0], _3, Queen[_0, _3]]]
    // implicitly[Map.Aux[QueenX[_0],
    //                    _1 :: _2 :: _3 :: Nil,
    //                    Queen[_0, _1] :: Queen[_0, _2] :: Queen[_0, _3] :: Nil]]

    // implicitly[QueensInRow.Aux[_0, _2, Queen[_0, _1] :: Queen[_0, _0] :: Nil]]

    // implicitly[Threatens.Aux[Queen[_0, _0], Queen[_10, _9], False]]
    // implicitly[Threatens.Aux[Queen[_0, _0], Queen[_1, _9], False]]
    // implicitly[Threatens.Aux[Queen[_0, _0], Queen[_0, _9], True]]
    // implicitly[Threatens.Aux[Queen[_0, _0], Queen[_9, _0], True]]
    // implicitly[Threatens.Aux[Queen[_0, _0], Queen[_10, _10], True]]

    // implicitly[Safe.Aux[Nil, Queen[_1, _2], True]]
    // implicitly[Safe.Aux[Queen[_0, _0] :: Nil, Queen[_0, _2], False]]
    // implicitly[Safe.Aux[Queen[_0, _0] :: Nil, Queen[_1, _2], True]]
    // implicitly[Safe.Aux[Queen[_0, _0] :: Queen[_1, _2] :: Queen[_3, _5] :: Nil,
    //                     Queen[_9, _6],
    //                     True]]

    // implicitly[
    //   AddQueen.Aux[
    //     _4,
    //     _1,
    //     Queen[_0, _0] :: Nil,
    //     (Queen[_1, _3] :: Queen[_0, _0] :: Nil) :: (Queen[_1, _2] :: Queen[
    //       _0,
    //       _0] :: Nil) :: Nil]
    // ]

    // implicitly[
    //   Map.Aux[
    //     AddQueen2[_4, _1],
    //     (Queen[_0, _0] :: Nil) :: Nil,
    //     ((Queen[_1, _3] :: Queen[_0, _0] :: Nil) :: (Queen[_1, _2] :: Queen[
    //       _0,
    //       _0] :: Nil) :: Nil) :: Nil]
    // ]

    // implicitly[AddQueenToAll.Aux[_4, _1, Nil, Nil]]
    // implicitly[
    //   AddQueenToAll.Aux[_4,
    //                     _1,
    //                     (Queen[_0, _0] :: Nil) :: Nil,
    //                     (Queen[_1, _3] :: Queen[_0, _0] :: Nil) :: (Queen[
    //                       _1,
    //                       _2] :: Queen[_0, _0] :: Nil) :: Nil]
    // ]

    // implicitly[
    //   AddQueensIf.Aux[False,
    //                   _10,
    //                   _7,
    //                   (Queen[_0, _0] :: Nil) :: Nil,
    //                   (Queen[_0, _0] :: Nil) :: Nil]]
    // implicitly[
    //   AddQueensIf.Aux[
    //     True,
    //     _4,
    //     _1,
    //     (Queen[_0, _0] :: Nil) :: Nil,
    //     (Queen[_1, _3] :: Queen[_0, _0] :: Nil) :: (Queen[_1, _2] :: Queen[
    //       _0,
    //       _0] :: Nil) :: Nil]]
  }
}
