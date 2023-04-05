//> using scala "3.2.1"

sealed trait Routine[A <: Tuple, B <: Tuple]:
  def execOn(stack : A, c : Char): B

case object Empty extends Routine[EmptyTuple, EmptyTuple]:
  def execOn(stack : EmptyTuple, c : Char): EmptyTuple = Tuple()
case class Push[E, T <: Tuple](x : E) extends Routine[T, E *: T]:
  def execOn(stack : T, c : Char): E *: T = x *: stack

case class PushChar[T <: Tuple]() extends Routine[T, Char *: T]:
  def execOn(stack : T, c : Char): Char *: T = c *: stack

case class ReducePair[T <: Tuple, X, Y, Z](
    op: (X, Y) => Z
) extends Routine[Y *: X *: T, Z *: T]:
  def execOn(stack : Y *: X *: T, c : Char): Z *: T =
    stack match
      case y *: x *: t => op(x,y) *: t
    
case class Transform[T <: Tuple, X, Y](op: X => Y)
  extends Routine[X *: T, Y *: T]:
    def execOn(stack : X *: T, c : Char): Y *: T =
      stack match
        case x *: stack => op(x) *: stack

case class Compose[T1 <: Tuple, T2 <: Tuple, T3 <: Tuple](r1 : Routine[T1, T2], r2: Routine[T2, T3])
  extends Routine[T1, T3]:
  def execOn(stack : T1, c : Char): T3 = r2.execOn(r1.execOn(stack, c), c)
      
//class State[T <: Tuple]
// sealed trait State[T <: Tuple, R]:
//   def exec(stack : T, word : List[Char]): Option[R] 
// class Accept extends State[Snoc[Lin, R], R]:
//   def exec(stack : Snoc[Lin, R], word : List[Char]): Option[R] = ???
// class NonAccepting()
// case class StateWithRoutine[I <: Tuple, O <: Tuple](state : State[O], routine: Routine[I, O])
// case class MooreMachine[R](
//   startStates : List[Option[State[EmptyTuple]]]
//   //next: [S] =>> (s : State[S]) => StateWithRoutine[S, O]
// )

// sealed trait HListWithBound[-T]
// case object Nil extends HListWithBound[Any]
// case class Cons[T, H <: T, TT <: HListWithBound[T]](head : H, tail : TT) extends HListWithBound[T]

// trait MooreMachine[R]:
//   type State
//   type SLookup[X <: State] <: Tuple
//   type Lookup[X <: Option[State]] <: Tuple = X match
//     case Some[s] => SLookup[s]
//     case None.type => Snoc[EmptyTuple, R]
//   val initStates : List[HListWithBound[Option[State]]]
//   def parse(word : List[Char]): Option[R]
  //val next : State => 
