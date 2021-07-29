package sandbox.regulareff

sealed trait Regex
final case class Empty() extends Regex
final case class Epsilon() extends Regex
final case class UU[I](value: I) extends Regex
final case class Star[I](regex: Regex) extends Regex

// // :.
trait Sequential extends Regex:
  type L <: Regex
  type R <: Regex

object Sequential:
  type Aux[LL, RR] = Sequential { type L = LL; type R = RR }

// // :||
trait Union extends Regex:
  type L <: Regex
  type R <: Regex

object Union:
  type Aux[LL, RR] = Union { type L = LL; type R = RR }

// // :&&
trait Intersection extends Regex:
  type L <: Regex
  type R <: Regex

object Intersection:
  type Aux[LL, RR] = Intersection { type L = LL; type R = RR }

// // TODO: Star and Unit might be wrong and needs type members
type Nu[R <: Regex] <: Regex = R match
  case Empty                  => Empty
  case Epsilon                => Epsilon
  case UU[i]                  => Empty
  case Sequential.Aux[l, r]   => Force[Intersection.Aux[Nu[l], Nu[r]]]
  case Intersection.Aux[l, r] => Force[Intersection.Aux[Nu[l], Nu[r]]]
  case Union.Aux[l, r]        => Force[Union.Aux[Nu[l], Nu[r]]]
  case Star[i]                => Epsilon

type Force[R <: Regex] <: Regex = R match
  case Empty                        => Empty
  case Epsilon                      => Epsilon
  case Intersection.Aux[Empty, r]   => Empty
  case Intersection.Aux[l, Empty]   => Empty
  case Intersection.Aux[Epsilon, r] => Force[r]
  case Intersection.Aux[l, Epsilon] => Force[l]
  case Union.Aux[Epsilon, r]        => Epsilon
  case Union.Aux[l, Epsilon]        => Epsilon
  case Union.Aux[Empty, r]          => Force[r]
  case Union.Aux[l, Empty]          => Force[l]

// type Delta[I, R <: Regex] <: Regex = R match
//   case Empty                  => Empty
//   case Epsilon                => Empty
//   case UU[I]                  => Epsilon
//   case UU[j]                  => Empty
//   case Sequential.Aux[l, r]   => Union.Aux[Delta[I, Sequential.Aux[l, r]], Sequential.Aux[Nu[l], Delta[I, r]]]
//   case Intersection.Aux[l, r] => Intersection.Aux[Delta[I, l], Delta[I, r]]
//   case Union.Aux[l, r]        => Union.Aux[Delta[I, l], Delta[I, r]]
//   case Star[i]                => Sequential.Aux[Delta[I, i], Star[i]]

sealed trait Free[F[_, _], R <: Regex, A]
final case class Pure[F[_, _], R <: Regex, A](value: A, ev: Nu[R] =:= Epsilon) extends Free[F, R, A]
// final case class Bind[F[_, _], I, R <: Regex, A, B](fa: F[I, A], cont: A => Free[F, Delta[I, R], B])
//     extends Free[F, R, B]

def pure[F[_, _], R <: Regex, A](value: A)(using ev: Nu[R] =:= Epsilon): Free[F, R, A] = Pure(value, ev)

sealed trait OpSort
final case class G() extends OpSort
final case class P() extends OpSort

sealed trait State[S, I, A]
final case class Get[S]() extends State[S, G, S]
final case class Put[S](s: S) extends State[S, P, Unit]

type GetPut = Star[Sequential.Aux[G, P]]

val pureExample: Free[[I, A] =>> State[Int, I, A], Epsilon, Int] = pure(1)

// def safe: Free[State[Int, ?, ?], GetPut, Unit] =
//   Bind(Get(), (s: Int) => Bind(Put(s + 1), (ss: Unit) => pure(())))

// def unsafe: Free[State[Int, *, *], GetPut, Unit] =
//   Bind(Get(), (s: Int) => Pure((), summon))
