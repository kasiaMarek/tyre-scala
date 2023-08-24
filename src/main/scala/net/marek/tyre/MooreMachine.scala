sealed trait Routine[IS <: Tuple, OS <: Tuple]:
  def execOn(stack: IS, c: Char): OS

case object Empty extends Routine[Nix, Nix]:
  def execOn(stack: Nix, c: Char): Nix = Tuple()

case class PushChar[IS <: Tuple]() extends Routine[IS, Char *: IS]:
  def execOn(stack: IS, c: Char): Char *: IS = c *: stack

case class Transform[IS <: Tuple, OS <: Tuple](op: IS => OS) extends Routine[IS, OS]:
  def execOn(stack: IS, c : Char): OS = op(stack)

case class Compose[IS <: Tuple, S <: Tuple, OS <: Tuple](r1: Routine[IS, S], r2: Routine[S, OS])
  extends Routine[IS, OS]:
  def execOn(stack: IS, c: Char): OS = r2.execOn(r1.execOn(stack, c), c)

case class OnTail[H, IS <: Tuple, OS <: Tuple](r: Routine[IS, OS]) extends Routine[H *: IS, H *: OS]:
  def execOn(stack: H *: IS, c: Char): H *: OS = stack match
    case h *: t => h *: r.execOn(t, c)

class Context[R <: Tuple]:
// --- states
  sealed trait State[S <: Tuple]:
    val next: List[RoutineNextState[S]]
    def test(c : Char): Boolean
    def id(s: S): S = s

  trait NonAcceptingState[S <: Tuple] extends State[S]

  object AcceptingState extends State[R]:
    val next: List[RoutineNextState[R]] = Nil
    def test(c: Char) = false

//--- routine next state
  type RoutineNextState[IS <: Tuple] = Either[RoutineAcceptingNextState[IS], RoutineNonAcceptingNextState[IS]]

  extension[IS <: Tuple] (r: RoutineNextState[IS])
    def thread(stack: IS, c: Char): Thread =
      r match
        case Left(r) => r.thread(stack, c)
        case Right(r) => r.thread(stack, c)

  trait RoutineNonAcceptingNextState[IS <: Tuple]:
    self =>
    type OS <: Tuple
    def routine: Routine[IS, OS]
    def nextState: NonAcceptingState[OS]
    def thread(stack: IS, c: Char): Thread =
      val newStack = routine.execOn(stack, c)
      new Thread:
        type S = OS
        def state = self.nextState
        def stack = newStack

  trait RoutineAcceptingNextState[IS <: Tuple]:
    self =>
    def routine: Routine[IS, R]
    def thread(stack: IS, c: Char): Thread =
      val newStack = routine.execOn(stack, c)
      new Thread:
        type S = R
        def state = AcceptingState
        def stack = newStack

//--- init states
  type InitState[IS <: Tuple] = Either[InitAcceptingState[IS], InitNonAcceptingState[IS]]

  extension[IS <: Tuple] (is: InitState[IS])
    def thread(initStack: IS): Thread =
      is match
        case Right(is) => is.thread(initStack)
        case Left(is) => is.thread(initStack)
      

  case class InitAcceptingState[IS <: Tuple](op: IS => R):
    def thread(initStack: IS): Thread =
      new Thread:
        type S = R
        def state = AcceptingState
        def stack = op(initStack)

  trait InitNonAcceptingState[IS <: Tuple]:
    self =>
    type OS <: Tuple
    def state: NonAcceptingState[OS]
    val op: IS => OS
    def thread(initStack: IS): Thread =
      new Thread:
        type S = OS
        def state = self.state
        def stack = op(initStack)

  trait Thread:
    type S <: Tuple
    def state: State[S]
    def stack: S
    def next(c: Char): List[Thread] =
      if(state.test(c))
      then state.next.map(_.thread(stack, c))
      else Nil
    def getIfAccepting: Option[R] =
      state match
        case AcceptingState => Some(AcceptingState.id(stack))
        case _ => None

  trait MooreMachine[IS <: Tuple]:
    val initStates: List[InitState[IS]]
    def parse(initStack: IS, word: List[Char]): Option[R] =
      def parseRec(word: List[Char], threads: List[Thread]): Option[R] =
        word match
          case c :: rest => parseRec(rest, threads.flatMap(_.next(c)))
          case Nil => threads.map(_.getIfAccepting).collectFirst:
            case Some(value) => value
      parseRec(word, initStates.map(_.thread(initStack)))
