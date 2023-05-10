object MMConstruction:
	
  def compile[T,R](tyre: Tyre[T], transform: T => R): MooreMachine[R] = tyre match
    case Pred(f) =>
      val initState = new StateWithInitRoutine[R]:
        type O = EmptyTuple
        val stateS = new State:
          val next: List[StateWithRoutine[EmptyTuple, R]] =
            val nextState = new StateWithRoutine[EmptyTuple, R]:
              type O = R *: EmptyTuple
              def stateS: State[O, R] = new AcceptingState[R]
              def routine: Routine[EmptyTuple, O] = Compose(PushChar(), Transform(transform))
            List(nextState)
          def test(c: Char) = f(c)
        def routine: InitRoutine[EmptyTuple, O] = Empty
      new MooreMachine[R]:
      	val initStates = List(initState)
    case Or(l,r) =>
      new MooreMachine:
        val initStates = compile(l, x => transform(Left(x))).initStates ++ 
        compile(r, x => transform(Right(x))).initStates
    case And(left,right) =>
      new MooreMachine:
        val cl = compile
        val initStates = ???
    case Star(l) => ???
    case Epsilon => 
      val initState = new StateWithInitRoutine[R]:
        type O = R *: EmptyTuple
        val stateS = AcceptingState[R]
        def routine: InitRoutine[EmptyTuple, O] = Push(transform(()))
      new MooreMachine[R]:
        val initStates = List(initState)
    case Conv(t, f) => compile(t, transform.compose(f))
