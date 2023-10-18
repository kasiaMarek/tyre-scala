package net.marek.tyre

class MMConstruction[IN <: Tuple, R](val context: Context[R *: IN]):
  import context._

  type Continuation[T, IS <: Tuple] = MooreMachine[T *: IS]
  type EmptyContinuation = MooreMachine[R *: IN]

  def compile[IS <: Tuple, T](tyre: Tyre[T], continuation: Continuation[T, IS]): MooreMachine[IS] = tyre match

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
          val op = identity
      new MooreMachine[IS]:
        val initStates = List(initState)

    case Or(l: Tyre[t1], r: Tyre[t2]) =>
      val mmL = continuation.contramap[t1 *: IS](x => Left(x.head) *: x.tail)
      val mmR = continuation.contramap[t2 *: IS](x => Right(x.head) *: x.tail)
      new MooreMachine:
        val initStates = compile[IS, t1](l, mmL).initStates ++ compile[IS, t2](r, mmR).initStates

    case And(l: Tyre[t1], r: Tyre[t2]) =>
      val mmT = continuation.contramap[t2 *: t1 *: IS](x => (x(1), x(0)) *: x.tail.tail)
      val mmF = compile[t1 *: IS, t2](r, mmT)
      compile[IS, t1](l, mmF)

    case Star(re: Tyre[t]) =>
      val ec: Context[t *: IS] = Context[t *: IS]
      val contr = MMConstruction(ec)
      val mmE = contr.compile[IS, t](re, contr.emptyContinuation)

      MMLoop[IS, t](contr.context, mmE).mmLoop(continuation)

    case Epsilon => continuation.contramap[IS](() *: _)

    case Conv(re: Tyre[t], f) =>
      val mm2 = continuation.contramap[t *: IS](x => f(x.head) *: x.tail)
      compile(re, mm2)

  def compile(tyre: Tyre[R]): MooreMachine[IN] = compile[IN, R](tyre, emptyContinuation)

  def emptyContinuation: EmptyContinuation =
    new EmptyContinuation:
      val initStates = List(new InitAcceptingState(identity))

  class MMLoop[IS <: Tuple, T](val context: Context[T *: IS], mmE: context.MooreMachine[IS]):

    def mmLoop(
      mm: MooreMachine[List[T] *: IS]
    ): MooreMachine[IS] =
      new MooreMachine[IS]:
        val initStates =
          lazy val fixableStates: List[RefinedInitNonAcceptingState[T, IS]] =
            mmE.initStates.flatMap:
              case is: context.InitAcceptingState[?] => Nil
              case is: context.InitNonAcceptingState[?] =>
                List:
                  new RefinedInitNonAcceptingState[T, IS]:
                    type Tail = is.OS
                    type OS = List[T] *: Tail
                    lazy val state = new NonAcceptingState:
                      val next: List[Transition[OS]] = is.state.next.flatMap(fixState[is.OS](fixableStates, mm, _))
                      def test(c: Char): Boolean = is.state.test(c)
                    val op = x => Nil *: is.op(x)
          fixableStates ++
            mm.initStates.map:
              case is: InitAcceptingState[?] =>
                new InitAcceptingState((x: IS) => is.op(Nil *: x))
              case is: InitNonAcceptingState[?] =>
                new InitNonAcceptingState[IS]:
                  type OS = is.OS
                  lazy val state = is.state
                  val op = x => is.op(Nil *: x)

    private def fixState[S <: Tuple](
      initStates: List[RefinedInitNonAcceptingState[T, IS]],
      continuation: Continuation[List[T], IS],
      rns0: context.Transition[S],
      alreadyFixed: List[AlreadyFixedStateMapping] = List.empty
    ): List[Transition[List[T] *: S]] =
      rns0 match
        case rns: context.AcceptingTransition[?] =>
          val mmIS = continuation.initStates.map:
            case is: InitAcceptingState[?] =>
              new AcceptingTransition[List[T] *: S]:
                lazy val routine: Routine[List[T] *: S, R *: IN] =
                  Compose[List[T] *: S, List[T] *: T *: IS, R *: IN](
                    OnTail(rns.routine),
                    Transform:
                      case l *: e *: t => is.op((l :+ e) *: t)
                  )
            case is: InitNonAcceptingState[?] =>
              new NonAcceptingTransition[List[T] *: S]:
                type OS = is.OS
                lazy val routine: Routine[List[T] *: S, OS] =
                  Compose[List[T] *: S, List[T] *: T *: IS, OS](
                    OnTail(rns.routine),
                    Transform:
                      case l *: e *: t => is.op((l :+ e) *: t)
                  )
                lazy val nextState: NonAcceptingState[OS] = is.state
          val mmEIS = initStates.map: is =>
            new NonAcceptingTransition[List[T] *: S]:
              type OS = is.OS
              lazy val routine: Routine[List[T] *: S, OS] =
                Compose[List[T] *: S, List[T] *: T *: IS, OS](
                  OnTail(rns.routine),
                  Transform:
                    case l *: e *: t =>
                      is.op(t) match
                        case empty *: tt =>
                          assert(empty == List(), empty)
                          (l :+ e) *: tt
                )
              lazy val nextState: NonAcceptingState[OS] = is.state
          mmIS ++ mmEIS
        case rns: context.NonAcceptingTransition[?] =>
          lazy val res: List[Transition[List[T] *: S]] = List:
            new NonAcceptingTransition[List[T] *: S]:
              type OS = List[T] *: rns.OS
              lazy val routine: Routine[List[T] *: S, OS] = OnTail(rns.routine)
              lazy val nextState: NonAcceptingState[OS] = new NonAcceptingState:
                val next: List[Transition[OS]] =
                  lazy val newFixed =
                    new AlreadyFixedStateMapping:
                      type B = S
                      def from = rns0
                      def to = res
                  rns.nextState.next.flatMap: next =>
                    alreadyFixed.getOrElse(next, fixState(initStates, continuation, next, newFixed :: alreadyFixed))
                def test(c: Char): Boolean = rns.nextState.test(c)
          res

    trait RefinedInitNonAcceptingState[T, IS <: Tuple] extends InitNonAcceptingState[IS]:
      type Tail <: Tuple
      type OS = List[T] *: Tail
      lazy val state: NonAcceptingState[OS]
      val op: IS => OS

    trait AlreadyFixedStateMapping:
      type B <: Tuple
      def get[A <: Tuple](rns: context.Transition[A]): Option[List[Transition[List[T] *: A]]] =
        if rns == from
        then Some(to.asInstanceOf[List[Transition[List[T] *: A]]])
        else None
      def from: context.Transition[B]
      def to: List[Transition[List[T] *: B]]

    extension (alreadyFixed: List[AlreadyFixedStateMapping])
      def getOrElse[A <: Tuple](
        ns: context.Transition[A],
        res: => List[Transition[List[T] *: A]]
      ): List[Transition[List[T] *: A]] =
        alreadyFixed.collectFirst(_.get(ns) match { case Some(v) => v }).getOrElse(res)
