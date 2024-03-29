package net.marek.tyre.automaton

import scala.collection.mutable
import net.marek.tyre.diagnostic.Renderer

/* Type parameters name conventions:
R - result type of parsing - parse tree shape
IS - input stack for routine
OS - output stack of routine
S - stack without input/output meaning
E - single element on stack
T - elementary TyRE type
 */

sealed private trait Routine[IS <: Tuple, OS <: Tuple]:
  def execOn(stack: IS, c: Char): OS

private case object Empty extends Routine[EmptyTuple, EmptyTuple]:
  def execOn(stack: EmptyTuple, c: Char): EmptyTuple = Tuple()

private case class PushChar[IS <: Tuple]() extends Routine[IS, Char *: IS]:
  def execOn(stack: IS, c: Char): Char *: IS = c *: stack

private case class Transform[IS <: Tuple, OS <: Tuple](op: IS => OS) extends Routine[IS, OS]:
  def execOn(stack: IS, c: Char): OS = op(stack)

private case class Compose[IS <: Tuple, S <: Tuple, OS <: Tuple](r1: Routine[IS, S], r2: Routine[S, OS])
  extends Routine[IS, OS]:
  def execOn(stack: IS, c: Char): OS = r2.execOn(r1.execOn(stack, c), c)

private case class OnTail[H, IS <: Tuple, OS <: Tuple](r: Routine[IS, OS]) extends Routine[H *: IS, H *: OS]:
  def execOn(stack: H *: IS, c: Char): H *: OS = stack match
    case h *: t => h *: r.execOn(t, c)

private class Context[R <: Tuple]:

  // States
  sealed trait State[S <: Tuple]:
    val next: List[Transition[S]]
    def test(c: Char): Boolean

  trait NonAcceptingState[S <: Tuple] extends State[S]

  object AcceptingState extends State[R]:
    val next: List[Transition[R]] = Nil
    def test(c: Char) = false

  // Transitions to next state, performing routines
  sealed trait Transition[IS <: Tuple]:
    def state: State[?]
    def thread(stack: IS, c: Char): Thread

  trait NonAcceptingTransition[IS <: Tuple] extends Transition[IS]:
    self =>
    type OS <: Tuple
    lazy val routine: Routine[IS, OS]
    lazy val nextState: NonAcceptingState[OS]
    def state = nextState
    def thread(stack: IS, c: Char): Thread =
      val newStack = routine.execOn(stack, c)
      new Thread:
        type S = OS
        lazy val state = self.nextState
        lazy val stack = newStack

  trait AcceptingTransition[IS <: Tuple] extends Transition[IS]:
    self =>
    lazy val routine: Routine[IS, R]
    def state = AcceptingState
    def thread(stack: IS, c: Char): Thread =
      val newStack = routine.execOn(stack, c)
      new Thread:
        type S = R
        lazy val state = AcceptingState
        lazy val stack = newStack

  // Init states
  sealed trait InitState[-IS <: Tuple]:
    type OS <: Tuple
    def state: State[?]
    def thread(initStack: IS): Thread
    def op(is: IS): OS

  trait InitNonAcceptingState[-IS <: Tuple] extends InitState[IS]:
    self =>
    lazy val state: NonAcceptingState[OS]
    def thread(initStack: IS): Thread =
      new Thread:
        type S = OS
        lazy val state = self.state
        lazy val stack = op(initStack)

  case class InitAcceptingState[-IS <: Tuple](opr: IS => R) extends InitState[IS]:
    type OS = R
    def state = AcceptingState
    def op(is: IS): OS = opr(is)
    def thread(initStack: IS): Thread =
      new Thread:
        type S = R
        lazy val state = AcceptingState
        lazy val stack = op(initStack)

  trait Thread:
    type S <: Tuple
    lazy val state: State[S]
    lazy val stack: S
    def next(c: Char): List[Thread] =
      if state.test(c)
      then state.next.map(_.thread(stack, c))
      else Nil
    def getIfAccepting: Option[R] = state match
      case AcceptingState => Some(stack)
      case _ => None

  trait Automaton[-IS <: Tuple]:

    val initStates: List[InitState[IS]]

    def parseAll(initStack: IS, word: List[Char]): List[R] =
      def parseRec(word: List[Char], threads: List[Thread]): List[R] =
        word match
          case c :: rest => parseRec(rest, threads.flatMap(_.next(c)).distinctBy(_.state))
          case Nil =>
            threads
              .map(_.getIfAccepting)
              .collect:
                case Some(value) => value
      parseRec(word, initStates.map(_.thread(initStack)))

    def parse(initStack: IS, word: List[Char]): Option[R] =
      def parseRec(word: List[Char], threads: List[Thread]): Option[R] =
        word match
          case c :: rest => parseRec(rest, threads.flatMap(_.next(c)).distinctBy(_.state))
          case Nil =>
            threads
              .map(_.getIfAccepting)
              .collectFirst:
                case Some(value) => value
      parseRec(word, initStates.map(_.thread(initStack)))

    def show(renderer: Renderer, testChars: Set[Char]): String =
      val visited = mutable.Set.empty[State[?]]
      def loop(states: List[State[?]]): Unit =
        visited.addAll(states)
        val nextStates =
          states.flatMap: st =>
            st.next.map: nextState =>
              renderer.add(st, nextState.state, testChars.filter(st.test))
              nextState.state
        nextStates.filterNot(visited(_)) match
          case Nil =>
          case some => loop(some)
      initStates.foreach(st => renderer.add(st.state))
      loop(initStates.map(_.state))
      renderer.render

    def contramap[IS1 <: Tuple](f: IS1 => IS): Automaton[IS1] =
      val iss: List[InitState[IS1]] =
        initStates.map:
          case is: InitAcceptingState[?] =>
            def op(x: IS1) = is.op(f(x))
            InitAcceptingState(op)
          case is: InitNonAcceptingState[?] =>
            new InitNonAcceptingState[IS1]:
              type OS = is.OS
              lazy val state: NonAcceptingState[OS] = is.state
              def op(x: IS1) = is.op(f(x))
      new Automaton[IS1]:
        val initStates = iss
