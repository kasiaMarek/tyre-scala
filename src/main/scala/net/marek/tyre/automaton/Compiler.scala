package net.marek.tyre.automaton

import net.marek.tyre.*

private[tyre] class TyreCompiler[IN <: Tuple, R](val context: Context[R *: IN]):
  import context._

  private type Continuation[-T, IS <: Tuple] = Automaton[T *: IS]
  private type EmptyContinuation = Automaton[R *: IN]

  private def compile[IS <: Tuple, T](tyre: Tyre[T], continuation: Continuation[T, IS]): Automaton[IS] = tyre match

    case Pred(f) =>
      val initState =
        new InitNonAcceptingState[IS]:
          type OS = IS
          lazy val state = new NonAcceptingState[IS]:
            val next: List[Transition[IS]] =
              continuation.initStates.map:
                case is: InitAcceptingState[?] =>
                  new AcceptingTransition[IS]:
                    lazy val routine = Compose(PushChar(), Transform(is.op))
                case is: InitNonAcceptingState[?] =>
                  new NonAcceptingTransition[IS]:
                    type OS = is.OS
                    lazy val nextState: NonAcceptingState[OS] = is.state
                    lazy val routine = Compose(PushChar(), Transform(is.op))
            def test(c: Char) = f(c)
          def op(x: IS) = x
      new Automaton[IS]:
        val initStates = List(initState)

    case Or(l: Tyre[t1], r: Tyre[t2]) =>
      val leftAdjustedCont = continuation.contramap[t1 *: IS](x => Left(x.head) *: x.tail)
      val rightAdjustedCont = continuation.contramap[t2 *: IS](x => Right(x.head) *: x.tail)
      new Automaton:
        val initStates =
          compile[IS, t1](l, leftAdjustedCont).initStates ++
            compile[IS, t2](r, rightAdjustedCont).initStates

    case And(l: Tyre[t1], r: Tyre[t2]) =>
      val adjustedCont = continuation.contramap[t2 *: t1 *: IS](x => (x(1), x(0)) *: x.tail.tail)
      val rightAutomaton = compile[t1 *: IS, t2](r, adjustedCont)
      compile[IS, t1](l, rightAutomaton)

    case Star(re: Tyre[t]) =>
      val ec: Context[t *: IS] = Context[t *: IS]
      val compiler = new TyreCompiler(ec)
      val innerAutomaton = compiler.compile[IS, t](re, compiler.emptyContinuation)

      Loop[IS, t](compiler.context, innerAutomaton, continuation).build

    case Epsilon => continuation.contramap[IS](() *: _)

    case Conv(re: Tyre[t], f) =>
      val adjustedCont = continuation.contramap[t *: IS](x => f(x.head) *: x.tail)
      compile(re, adjustedCont)

  def compile(tyre: Tyre[R]): Automaton[IN] = compile[IN, R](tyre, emptyContinuation)

  private def emptyContinuation: EmptyContinuation =
    new EmptyContinuation:
      val initStates = List(new InitAcceptingState(identity))

  private class Loop[IS <: Tuple, T](
    val context: Context[T *: IS],
    innerAutomaton: context.Automaton[IS],
    continuation: Continuation[List[T], IS]
  ):

    def build: Automaton[IS] =
      new Automaton[IS]:
        val initStates =
          lazy val fixableStates: List[RefinedInitNonAcceptingState[T, IS]] =
            innerAutomaton.initStates.flatMap:
              case is: context.InitAcceptingState[?] => Nil
              case is: context.InitNonAcceptingState[?] =>
                List:
                  new RefinedInitNonAcceptingState[T, IS]:
                    type Tail = is.OS
                    type OS = List[T] *: Tail
                    lazy val state = new NonAcceptingState:
                      val next: List[Transition[OS]] =
                        is.state.next.flatMap(fixTransition[is.OS](fixableStates, _))
                      def test(c: Char): Boolean = is.state.test(c)
                    def opTail(x: IS) = is.op(x)
          val continuationStates =
            continuation.initStates.map:
              case is: InitAcceptingState[?] =>
                new InitAcceptingState((x: IS) => is.op(Nil *: x))
              case is: InitNonAcceptingState[?] =>
                new InitNonAcceptingState[IS]:
                  type OS = is.OS
                  lazy val state = is.state
                  def op(x: IS) = is.op(Nil *: x)
          fixableStates ++ continuationStates

    private def fixTransition[S <: Tuple](
      initStates: List[RefinedInitNonAcceptingState[T, IS]],
      transition: context.Transition[S],
      alreadyFixed: List[AlreadyFixedTransition] = List.empty
    ): List[Transition[List[T] *: S]] =
      transition match
        case transition: context.AcceptingTransition[?] =>
          fixAcceptingTransition(initStates, transition)
        case transition: context.NonAcceptingTransition[?] =>
          fixNonAcceptingTransition(initStates, transition, alreadyFixed)

    private def fixAcceptingTransition[S <: Tuple](
      initStates: List[RefinedInitNonAcceptingState[T, IS]],
      transition: context.AcceptingTransition[S]
    ): List[Transition[List[T] *: S]] =
      val withContinuation = continuation.initStates.map:
        case is: InitAcceptingState[?] =>
          new AcceptingTransition[List[T] *: S]:
            lazy val routine: Routine[List[T] *: S, R *: IN] =
              Compose[List[T] *: S, List[T] *: T *: IS, R *: IN](
                OnTail(transition.routine),
                Transform:
                  case l *: e *: t => is.op((l :+ e) *: t)
              )
        case is: InitNonAcceptingState[?] =>
          transitionWithTail(transition, is):
            case l *: e *: t => is.op((l :+ e) *: t)
      val looped = initStates.map: is =>
        transitionWithTail(transition, is):
          case l *: e *: t =>
            is.op(t) match
              case Nil *: tt => (l :+ e) *: tt
      looped ++ withContinuation

    private def transitionWithTail[S <: Tuple, A <: Tuple](
      head: context.AcceptingTransition[S],
      tail: InitNonAcceptingState[A]
    )(transform: List[T] *: T *: IS => tail.OS): Transition[List[T] *: S] =
      new NonAcceptingTransition[List[T] *: S]:
        type OS = tail.OS
        lazy val routine: Routine[List[T] *: S, OS] =
          Compose[List[T] *: S, List[T] *: T *: IS, OS](
            OnTail(head.routine),
            Transform(transform)
          )
        lazy val nextState: NonAcceptingState[OS] = tail.state

    private def fixNonAcceptingTransition[S <: Tuple](
      initStates: List[RefinedInitNonAcceptingState[T, IS]],
      transition: context.NonAcceptingTransition[S],
      alreadyFixed: List[AlreadyFixedTransition]
    ): List[Transition[List[T] *: S]] =
      lazy val fixedTransitions: List[Transition[List[T] *: S]] = List:
        new NonAcceptingTransition[List[T] *: S]:
          type OS = List[T] *: transition.OS
          lazy val routine: Routine[List[T] *: S, OS] = OnTail(transition.routine)
          lazy val nextState: NonAcceptingState[OS] = new NonAcceptingState:
            val next: List[Transition[OS]] =
              lazy val newFixed =
                new AlreadyFixedTransition:
                  type B = S
                  def from = transition
                  def to = fixedTransitions
              transition.nextState.next.flatMap: next =>
                alreadyFixed.getOrElse(next, fixTransition(initStates, next, newFixed :: alreadyFixed))
            def test(c: Char): Boolean = transition.nextState.test(c)
      fixedTransitions

    private trait RefinedInitNonAcceptingState[T, IS <: Tuple] extends InitNonAcceptingState[IS]:
      type Tail <: Tuple
      type OS = List[T] *: Tail
      lazy val state: NonAcceptingState[OS]
      def op(x: IS): Nil.type *: Tail = Nil *: opTail(x)
      def opTail(x: IS): Tail

    protected trait AlreadyFixedTransition:
      type B <: Tuple
      def get[A <: Tuple](transition: context.Transition[A]): Option[List[Transition[List[T] *: A]]] =
        if transition == from
        then Some(to.asInstanceOf[List[Transition[List[T] *: A]]])
        else None
      def from: context.Transition[B]
      def to: List[Transition[List[T] *: B]]

    extension (alreadyFixed: List[AlreadyFixedTransition])
      private def getOrElse[A <: Tuple](
        transition: context.Transition[A],
        default: => List[Transition[List[T] *: A]]
      ): List[Transition[List[T] *: A]] =
        alreadyFixed.collectFirst(_.get(transition) match { case Some(v) => v }).getOrElse(default)

private[tyre] object TyreCompiler:
  def apply[T]: TyreCompiler[EmptyTuple, T] = new TyreCompiler(Context[T *: EmptyTuple])
