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


trait StateWithRoutine[I <: Tuple, R <: Tuple]:
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

trait State[S <: Tuple, R <: Tuple]:
  def next(c : Char): List[StateWithRoutine[S, R]]

trait AcceptingState[R <: Tuple] extends State[R, R]:
  def next(c : Char): List[StateWithRoutine[R, R]]

trait Thread[R <: Tuple]:
  type S <: Tuple
  def state: State[S, R]
  def stack: S
  def next(c : Char): List[Thread[R]] =
    state.next(c).map(_.thread(stack, c))
  def getIfAccepting: Option[R] =
    state match {
      case _ : AcceptingState[_] => Some(stack)
      case _ => None
    }

trait MooreMachine[R]:
  val initStates : List[StateWithRoutine[EmptyTuple, R *: EmptyTuple]]
  def parse(word : List[Char]): Option[R] =
    def parseRec(word : List[Char], threads : List[Thread[R *: EmptyTuple]]): Option[R] =
      word match
        case c :: rest => parseRec(rest, threads.flatMap(_.next(c)))
        case Nil => threads.map(_.getIfAccepting).collect{
          case Some(value) => value._1
        }.headOption
    parseRec(word, initStates.map(_.thread(Tuple.apply(), 'a')))
