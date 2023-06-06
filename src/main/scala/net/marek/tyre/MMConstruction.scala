object MMConstruction:
	
  def compile[IS <: Tuple, T, R](tyre: Tyre[T], mm: MooreMachine[T *: IS, R]): MooreMachine[IS, R] = tyre match
    case Pred(f) =>
      val initState = new InitState[IS, R]:
        type OS = IS
        val state = new State:
          val next: List[RoutineNextState[IS, R]] =
            mm.initStates.map: is => 
              new RoutineNextState[IS, R]:
                type OS = is.OS
                def nextState: State[OS, R] = is.state
                def routine: Routine[IS, OS] = Compose(PushChar(), Transform(is.op))
          def test(c: Char) = f(c)
        val op = identity
      new MooreMachine[IS, R]:
      	val initStates = List(initState)
    case Or(l: Tyre[t1], r: Tyre[t2]) =>
      val mmL = mmMap[T, t1, IS, R](x => Left(x), mm)
      val mmR = mmMap[T, t2, IS, R](x => Right(x), mm)
      new MooreMachine:
        val initStates = compile[IS, t1, R](l, mmL).initStates ++ compile[IS, t2, R](r, mmR).initStates
    case And(l: Tyre[t1], r: Tyre[t2]) =>
      val mmT = new MooreMachine[t2 *: t1 *: IS, R]:
        val initStates = mm.initStates.map: is =>
          new InitState[t2 *: t1 *: IS, R]:
            type OS = is.OS
            def state = is.state
            val op = x => is.op((x(1), x(0)) *: x.tail.tail)
      val mmF = compile[t1 *: IS, t2, R](r, mmT)
      compile[IS, t1, R](l, mmF)
    case Star(re: Tyre[t]) =>
      val mmE = mmMap[T, t, IS, R](_ => Nil, mm)
      val mmS = mmMap[T, t, IS, R](x => List(x), mm)
      val mmI = new MooreMachine[t *: IS, R]:
        val initStates = mmE.initStates ++ mmS.initStates
      val mmL = compile(re, mmI)
      mmLoop[T, IS, R](mmL)
    case Epsilon =>
      new MooreMachine[IS, R]:
        val initStates = mm.initStates.map: is =>
          new InitState[IS, R]:
            type OS = is.OS
            def state = is.state
            val op = x => is.op(() *: x)
    case Conv(re: Tyre[t], f) =>
      val mm2 = mmMap[T, t, IS, R](f, mm)
      compile(re, mm2)

  def compile[R](tyre: Tyre[R]): MooreMachine[Nix, R] =
    val mm = mmSeed[RS[R], R](_.head)
    compile(tyre, mm)

  def mmSeed[IS <: Tuple, R](op: IS => R): MooreMachine[IS, R] =
    val initState = new InitState[IS, R]:
      type OS = RS[R]
      val state = new AcceptingState[R]
      val op = op
    new MooreMachine[IS, R]:
      val initStates = List(initState)

  def mmMap[T1, T2, IS <: Tuple, R](f: T2 => T1, mm: MooreMachine[T1 *: IS, R]): MooreMachine[T2 *: IS, R] =
    val initStates =
      mm.initStates.map: is =>
        new InitState[T2 *: IS, R]:
          type OS = is.OS
          def state: State[OS, R] = is.state
          val op = x => is.op(f(x.head) *: x.tail)
    new MooreMachine[T2 *: IS, R]:
      val initStates = initStates

  def mmLoop[T, IS <: Tuple, R](mm: MooreMachine[IS, R]): MooreMachine[IS, R] = ???

  def loop[IS <: Tuple, T, R](mm: MooreMachine[IS, T *: IS], cont: MooreMachine[List[T] *: IS, R]): MooreMachine[IS, R] =
    def fixState[OS <: Tuple](state : State[OS, T *: IS]): List[State[List[T] *: OS, R]] =
      state match
        case a : AcceptingState[_] => ???
        case s => ???
      
    new MooreMachine[IS, R]:
      val initStates =
        mm.initStates.flatMap: is =>
          fixState(is.state).map: fixedState =>
            new InitState[IS, R]:
              type OS = List[T] *: is.OS
              def state = fixedState
              val op = x => Nil *: is.op(x)


