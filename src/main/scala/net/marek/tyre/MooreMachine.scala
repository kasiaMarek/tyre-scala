//> using scala "3.3.0-RC3"

sealed trait Routine[A <: Tuple, B <: Tuple]:
  def execOn(stack: A, c: Char): B

sealed trait InitRoutine[A <: Tuple, B <: Tuple] extends Routine[A, B]:
  def execOn(stack: A, c: Char): B = execOn(stack)
  def execOn(stack: A): B

case object Empty extends InitRoutine[EmptyTuple, EmptyTuple]:
  def execOn(stack: EmptyTuple): EmptyTuple = Tuple()

case class Push[E, T <: Tuple](x: E) extends InitRoutine[T, E *: T]:
  def execOn(stack: T): E *: T = x *: stack

case class PushChar[T <: Tuple]() extends Routine[T, Char *: T]:
  def execOn(stack: T, c: Char): Char *: T = c *: stack

case class ReducePair[T <: Tuple, X, Y, Z](op: (X, Y) => Z) extends InitRoutine[Y *: X *: T, Z *: T]:
  def execOn(stack: Y *: X *: T): Z *: T =
    stack match
      case y *: x *: t => op(x, y) *: t

case class Transform[T <: Tuple, X, Y](op: X => Y) extends InitRoutine[X *: T, Y *: T]:
  def execOn(stack: X *: T): Y *: T =
    stack match
      case x *: stack => op(x) *: stack

case class Compose[T1 <: Tuple, T2 <: Tuple, T3 <: Tuple](r1: Routine[T1, T2], r2: Routine[T2, T3])
  extends Routine[T1, T3]:
  def execOn(stack: T1, c: Char): T3 = r2.execOn(r1.execOn(stack, c), c)

case class ComposeInit[T1 <: Tuple, T2 <: Tuple, T3 <: Tuple](r1: InitRoutine[T1, T2], r2: InitRoutine[T2, T3])
  extends InitRoutine[T1, T3]:
  def execOn(stack: T1): T3 = r2.execOn(r1.execOn(stack))


trait StateWithRoutine[I <: Tuple, R]:
  type O <: Tuple
  def stateS: State[O, R]
  def routine: Routine[I, O]
  def thread(stack : I, c : Char): Thread[R] =
    val newStack = routine.execOn(stack, c)
    new Thread {
      type S = O
      def state = stateS
      def stack = newStack
    }

trait StateWithInitRoutine[R]:
  type O <: Tuple
  def stateS: State[O, R]
  def routine: InitRoutine[EmptyTuple, O]
  def thread: Thread[R] =
    val newStack = routine.execOn(Tuple.apply())
    new Thread {
      type S = O
      def state = stateS
      def stack = newStack
    }

trait State[S <: Tuple, R]:
  val next: List[StateWithRoutine[S, R]]
  def test(c : Char): Boolean
  def id(s: S): S = s

class AcceptingState[R] extends State[R *: EmptyTuple, R]:
  val next: List[StateWithRoutine[R *: EmptyTuple, R]] = Nil
  def test(c : Char) = false

trait Thread[R]:
  type S <: Tuple
  def state: State[S, R]
  def stack: S
  def next(c : Char): List[Thread[R]] =
    if(state.test(c))
    then state.next.map(_.thread(stack, c))
    else Nil
  def getIfAccepting: Option[R] =
    state match
      case a : AcceptingState[R @unchecked] =>
        Some(a.id(stack)(0))
      case _ => None

trait MooreMachine[R]:
  val initStates : List[StateWithInitRoutine[R]]
  def parse(word : List[Char]): Option[R] =
    def parseRec(word : List[Char], threads : List[Thread[R]]): Option[R] =
      word match
        case c :: rest => parseRec(rest, threads.flatMap(_.next(c)))
        case Nil => threads.map(_.getIfAccepting).collectFirst:
          case Some(value) => value
    parseRec(word, initStates.map(_.thread))
