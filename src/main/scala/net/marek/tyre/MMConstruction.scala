import scala.quoted.runtime.Patterns.fromAbove
class MMConstruction[IN <: Tuple, R](val context: Context[R *: IN]):
  import context._

  def compile[IS <: Tuple, T](tyre: Tyre[T], mm: MooreMachine[T *: IS]): MooreMachine[IS] = tyre match
    case OneOf(cs) =>
      val initState =
        new InitNonAcceptingState[IS]:
          type OS = IS
          val state = new NonAcceptingState[IS]:
            val next: List[RoutineNextState[IS]] =
              mm.initStates.map:
                case Left(is) =>
                  Left:
                    new RoutineAcceptingNextState[IS]:
                      def routine = Compose(PushChar(), Transform(is.op))
                case Right(is) =>
                  Right:
                    new RoutineNonAcceptingNextState[IS]:
                      type OS = is.OS
                      def nextState: NonAcceptingState[OS] = is.state
                      def routine = Compose(PushChar(), Transform(is.op))
            def test(c: Char) = cs.contains(c)
          val op = identity
      new MooreMachine[IS]:
        val initStates = List(Right(initState))
    case Or(l: Tyre[t1], r: Tyre[t2]) =>
      val mmL = mmMap[T, t1, IS](x => Left(x), mm)
      val mmR = mmMap[T, t2, IS](x => Right(x), mm)
      new MooreMachine:
        val initStates = compile[IS, t1](l, mmL).initStates ++ compile[IS, t2](r, mmR).initStates
    case And(l: Tyre[t1], r: Tyre[t2]) =>
      val mmT = new MooreMachine[t2 *: t1 *: IS]:
        val initStates = mm.initStates.map:
          case Left(is) =>
            Left(new InitAcceptingState[t2 *: t1 *: IS](x => is.op((x(1), x(0)) *: x.tail.tail)))
          case Right(is) =>
            Right:
              new InitNonAcceptingState[t2 *: t1 *: IS]:
                type OS = is.OS
                def state = is.state
                val op = x => is.op((x(1), x(0)) *: x.tail.tail)
      val mmF = compile[t1 *: IS, t2](r, mmT)
      compile[IS, t1](l, mmF)
    case Star(re: Tyre[t]) =>
      val ec: Context[t *: IS] = Context[t *: IS]
      val contr = MMConstruction(ec)
      val mmE = contr.compile[IS, t](re, contr.seedMM)

      MMLoop[IS, t](contr.context, mmE).mmLoop(mm)

    case Epsilon =>
      new MooreMachine[IS]:
        val initStates = mm.initStates.map:
          case Left(is) => Left(new InitAcceptingState(x => is.op(() *: x)))
          case Right(is) =>
            Right:
              new InitNonAcceptingState[IS]:
                type OS = is.OS
                def state = is.state
                val op = x => is.op(() *: x)

    case Conv(re: Tyre[t], f) =>
      val mm2 = mmMap[T, t, IS](f, mm)
      compile(re, mm2)

  def compile(tyre: Tyre[R]): MooreMachine[IN] = compile[IN, R](tyre, seedMM)

  def seedMM: MooreMachine[R *: IN] =
    val initState = new InitAcceptingState[R *: IN](identity)
    new MooreMachine[R *: IN]:
      val initStates = List(Left(initState))

  def mmMap[T1, T2, IS <: Tuple](f: T2 => T1, mm: MooreMachine[T1 *: IS]): MooreMachine[T2 *: IS] =
    val iss =
      mm.initStates.map:
        case Left(is) =>
          def op(x : T2 *: IS) = is.op(f(x.head) *: x.tail)
          Left(InitAcceptingState(op))
        case Right(is) =>
          Right:
            new InitNonAcceptingState[T2 *: IS]:
              type OS = is.OS
              def state: NonAcceptingState[OS] = is.state
              val op = x => is.op(f(x.head) *: x.tail)
    new MooreMachine[T2 *: IS]:
      val initStates = iss

  class MMLoop[IS <: Tuple, T](val context: Context[T *: IS], mmE: context.MooreMachine[IS]):
    def mmLoop(
      mm: MooreMachine[List[T] *: IS]
    ): MooreMachine[IS] =

      new MooreMachine[IS]:
        val initStates =
          mmE.initStates.flatMap:
            case Left(_) => Nil
            case Right(is)=>
              List:
                Right:
                  new InitNonAcceptingState[IS]:
                    type OS = List[T] *: is.OS
                    def state = new NonAcceptingState:
                      val next: List[RoutineNextState[OS]] = is.state.next.flatMap(fixState[is.OS](initStates, mm, _))
                      def test(c: Char): Boolean = is.state.test(c)
                    val op = x => Nil *: is.op(x)
          ++
          mm.initStates.map:
            case Left(is) => Left(new InitAcceptingState((x: IS) => is.op(Nil *: x)))
            case Right(is) =>
              Right:
                new InitNonAcceptingState[IS]:
                  type OS = is.OS
                  def state = is.state
                  val op = x => is.op(Nil *: x)


    private def fixState[S <: Tuple](
      initStates: List[InitState[IS]],
      mm: MooreMachine[List[T] *: IS],
      rns0: context.RoutineNextState[S],
      alreadyFixed: List[AlreadyFixedStateMapping] = List.empty
    ): List[RoutineNextState[List[T] *: S]] =
      rns0 match
        case Left(rns) =>
          mm.initStates.map:
            case Left(is) =>
              Left:
                new RoutineAcceptingNextState[List[T] *: S]:
                  def routine: Routine[List[T] *: S, R *: IN] =
                    Compose[List[T] *: S, List[T] *: T *: IS, R *: IN](
                      OnTail(rns.routine),
                      Transform:
                        case l *: e *: t => is.op((l :+ e) *: t)
                    )
            case Right(is) =>
              Right:
                new RoutineNonAcceptingNextState[List[T] *: S]:
                  type OS = is.OS
                  def routine: Routine[List[T] *: S, OS] =
                    Compose[List[T] *: S, List[T] *: T *: IS, OS](
                      OnTail(rns.routine),
                      Transform:
                        case l *: e *: t => is.op((l :+ e) *: t)
                    )
                  def nextState: NonAcceptingState[OS] = is.state
          ++
          initStates.map:
            case Left(is) =>
              Left:
                new RoutineAcceptingNextState[List[T] *: S]:
                  def routine: Routine[List[T] *: S, R *: IN] =
                    Compose[List[T] *: S, List[T] *: T *: IS, R *: IN](
                      OnTail(rns.routine),
                      Transform:
                        case l *: e *: t => is.op(t) match
                          case _ *: tt => ((l :+ e).asInstanceOf[R] *: tt)
                    )
            case Right(is) =>
              Right:
                new RoutineNonAcceptingNextState[List[T] *: S]:
                  type OS = is.OS
                  def routine: Routine[List[T] *: S, OS] =
                    Compose[List[T] *: S, List[T] *: T *: IS, OS](
                      OnTail(rns.routine),
                      Transform:
                        case l *: e *: t => (is.op(t): @unchecked) match
                          case _ *: tt => ((l :+ e) *: tt).asInstanceOf[OS]
                    )
                  def nextState: NonAcceptingState[OS] = is.state
        case Right(rns) =>
          def res: List[RoutineNextState[List[T] *: S]] = List:
            Right:
              new RoutineNonAcceptingNextState[List[T] *: S]:
                type OS = List[T] *: rns.OS
                def routine: Routine[List[T] *: S, OS] = OnTail(rns.routine)
                def nextState: NonAcceptingState[OS] = new NonAcceptingState:
                  val next: List[RoutineNextState[OS]] =
                    lazy val newFixed =
                      new AlreadyFixedStateMapping:
                        type B = S
                        def from = rns0
                        def to = res
                    rns.nextState.next.flatMap: next =>
                      alreadyFixed.getOrElse(next, fixState(initStates, mm, next, newFixed :: alreadyFixed))
                  def test(c: Char): Boolean = rns.nextState.test(c)
          res


    trait AlreadyFixedStateMapping:
      type B <: Tuple
      def get[A <: Tuple](rns: context.RoutineNextState[A]): Option[List[RoutineNextState[List[T] *: A]]] =
        if rns == from then Some(to.asInstanceOf[List[RoutineNextState[List[T] *: A]]]) else None
      def from: context.RoutineNextState[B]
      def to: List[RoutineNextState[List[T] *: B]]

    extension(alreadyFixed: List[AlreadyFixedStateMapping])
      def getOrElse[A <: Tuple](
        ns: context.RoutineNextState[A],
        res: => List[RoutineNextState[List[T] *: A]]
      ): List[RoutineNextState[List[T] *: A]] =
        alreadyFixed.collectFirst(_.get(ns) match { case Some(v) => v }).getOrElse(res)

