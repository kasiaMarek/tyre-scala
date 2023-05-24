object MMConstruction:
	
  def compile[I <: Tuple, T,R](tyre: Tyre[T], mm: MooreMachine[T *: I, R]): MooreMachine[I, R] = tyre match
    case Pred(f) =>
      val initState = new InitState[I, R]:
        type O = I
        val state = new State:
          val next: List[StateWithRoutine[I, R]] =
            mm.initStates.map: is => 
              new StateWithRoutine[I, R]:
                type O = is.O
                def state: State[O, R] = is.state
                def routine: Routine[I, O] = Compose(PushChar(), Transform1(is.op))
          def test(c: Char) = f(c)
        val op = identity
      new MooreMachine[I, R]:
      	val initStates = List(initState)
    case Or(l: Tyre[a], r: Tyre[b]) =>
      val mm1 = mmMap[T,a,I,R](x => Left(x), mm)
      val mm2 = mmMap[T,b,I,R](x => Right(x), mm)
      new MooreMachine:
        val initStates = compile(l, mm1).initStates ++ compile(r, mm2).initStates
    case And(l: Tyre[a],r: Tyre[b]) =>
      val mtail = new MooreMachine[b *: a *: I, R]:
        val initStates = mm.initStates.map: is =>
          new InitState[b *: a *: I, R]:
            type O = is.O
            def state = is.state
            val op = x => is.op((x(1), x(0)) *: x.tail.tail)
      val mm2 = compile[a *: I, b, R](r, mtail)
      compile(l, mm2)
    case Star(l) => ???
    case Epsilon =>
      new MooreMachine[I, R]:
        val initStates = mm.initStates.map: is =>
          new InitState[I, R]:
            type O = is.O
            def state = is.state
            val op = x => is.op(() *: x)
    case Conv(t: Tyre[a], f) =>
      val mm2 = mmMap[T, a, I, R](f, mm)
      compile(t, mm2)

  def compile[T](tyre: Tyre[T]): MooreMachine[EmptyTuple, T] =
    val initState = new InitState[T *: EmptyTuple, T]:
      type O = T *: EmptyTuple
      val state = new AcceptingState[T]
      val op = identity
    val mm = new MooreMachine[T *: EmptyTuple, T]:
        val initStates = List(initState)
    compile(tyre, mm)

  def mmMap[A, B, I <: Tuple, T](f: B => A, mm: MooreMachine[A *: I,T]): MooreMachine[B *: I, T] =
    val initStates =
      mm.initStates.map: is =>
        new InitState[B *: I, T]:
          type O = is.O
          def state: State[O, T] = is.state
          val op = x => is.op(f(x.head) *: x.tail)
    new MooreMachine[B *: I, T]:
      val initStates = initStates

  def mmm[I <: Tuple, T](op: I => T): MooreMachine[I, T] =
    val initState = new InitState[I, T]:
      type O = T *: EmptyTuple
      val state = new AcceptingState[T]
      val op = op
    new MooreMachine[I, T]:
      val initStates = List(initState)


