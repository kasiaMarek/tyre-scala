//> using scala "3.3.0-RC3"

//Tyre
sealed trait Tyre[T]
//  def compile(): MooreMachine[T]
case class Pred(f: Char => Boolean) extends Tyre[Char]
case class Or[A, B](right: Tyre[A], left: Tyre[B]) extends Tyre[Either[A, B]]
case class And[A, B](right: Tyre[A], left: Tyre[B]) extends Tyre[(A, B)]
case class Star[A](re: Tyre[A]) extends Tyre[List[A]]
case object Epsilon extends Tyre[Unit]
case class Conv[A,B](tyre: Tyre[A], f: A => B) extends Tyre[B]



















//Heterogenous stack
// sealed trait TStack
// class SNil extends TStack
// case class Cons[F, T <: TStack](head: F, tail: T) extends TStack

// sealed trait Op[A <: TStack, B <: TStack]
// case class Push[A <: TStack, E](e: E) extends Op[A, Cons[E, A]]
// case class Red[A <: TStack, E, F, G](f: E => F => G)
//     extends Op[Cons[E, Cons[F, A]], Cons[G, A]]
// case class Trans[A <: TStack, E, G](f: E => G)
//     extends Op[Cons[E, A], Cons[G, A]]

// sealed trait State[A <: TStack, B]
// class Accept[B] extends State[SNil, B]
// case class Item[A <: TStack, B](c: Char, st: State[A, B]) extends State[A, B]
// case class ReducePair[A <: TStack, B, C <: TStack](
//     op: Op[A, C],
//     st: State[C, B]
// ) extends State[A, B]
// case class Split[A <: TStack, B](st1: State[A, B], st2: State[A, B])
//     extends State[A, B]
//Fail :: State xs res
//Instructions
// sealed trait Routine[A, B <: TStack]:
//   def exec(in: A, c: Char): B

// class PushChar[A <: TStack] extends Instruction[A, Cons[Char, A]]:
//   def exec(in: A, c: Char): Cons[Char, A] = Cons(c, in)

// class Transform[A <: TStack, EA, EB](f: EA => EB)
//     extends Instruction[Cons[EA, A], Cons[EB, A]]:
//   def exec(in: Cons[EA, A], c: Char): Cons[EB, A] =
//     in match
//       case Cons(head, tail) => Cons(f(head), tail)
