sealed trait Routine[IS <: Tuple, OS <: Tuple]:
  def execOn(stack: IS, c: Char): OS

case object Empty extends Routine[Nix, Nix]:
  def execOn(stack: Nix, c: Char): Nix = Tuple()

case class Push[E, IS <: Tuple](e: E) extends Routine[IS, E *: IS]:
  def execOn(stack: IS, c: Char): E *: IS = e *: stack

case class PushChar[IS <: Tuple]() extends Routine[IS, Char *: IS]:
  def execOn(stack: IS, c: Char): Char *: IS = c *: stack

case class ReducePair[S <: Tuple, E1, E2, E3](op: (E1, E2) => E3) extends Routine[E2 *: E1 *: S, E3 *: S]:
  def execOn(stack: E2 *: E1 *: S, c: Char): E3 *: S =
    stack match
      case e2 *: e1 *: s => op(e1, e2) *: s

case class Transform[IS <: Tuple, OS <: Tuple](op: IS => OS) extends Routine[IS, OS]:
  def execOn(stack: IS, c : Char): OS = op(stack)

case class Compose[IS <: Tuple, S <: Tuple, OS <: Tuple](r1: Routine[IS, S], r2: Routine[S, OS])
  extends Routine[IS, OS]:
  def execOn(stack: IS, c: Char): OS = r2.execOn(r1.execOn(stack, c), c)


trait RoutineToState[IS <: Tuple, R]:
  self =>
  type OS <: Tuple
  def routine: Routine[IS, OS]
  def state: State[OS, R]
  def thread(stack: IS, c : Char): Thread[R] =
    val newStack = routine.execOn(stack, c)
    new Thread:
      type S = OS
      def state = self.state
      def stack = newStack

trait State[S <: Tuple, R]:
  val next: List[RoutineToState[S, R]]
  def test(c : Char): Boolean
  def id(s: S): S = s

trait InitState[IS <: Tuple, R]:
  self =>
  type OS <: Tuple
  def state: State[OS, R]
  val op: IS => OS
  def thread(initStack: IS): Thread[R] =
    new Thread:
      type S = OS
      def state = self.state
      def stack = op(initStack)

class AcceptingState[R] extends State[RS[R], R]:
  val next: List[RoutineToState[RS[R], R]] = Nil
  def test(c: Char) = false

trait Thread[R]:
  type S <: Tuple
  def state: State[S, R]
  def stack: S
  def next(c: Char): List[Thread[R]] =
    if(state.test(c))
    then state.next.map(_.thread(stack, c))
    else Nil
  def getIfAccepting: Option[R] =
    state match
      case a: AcceptingState[R @unchecked] =>
        Some(a.id(stack)(0))
      case _ => None

trait MooreMachine[IS <: Tuple, R]:
  val initStates: List[InitState[IS, R]]
  def parse(initStack: IS, word: List[Char]): Option[R] =
    def parseRec(word: List[Char], threads: List[Thread[R]]): Option[R] =
      word match
        case c :: rest => parseRec(rest, threads.flatMap(_.next(c)))
        case Nil => threads.map(_.getIfAccepting).collectFirst:
          case Some(value) => value
    parseRec(word, initStates.map(_.thread(initStack)))
