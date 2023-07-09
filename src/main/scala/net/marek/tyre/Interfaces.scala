trait Machine[T]:
  def run(str: String): Option[T]

extension[T] (regex: Tyre[T])
  def compile(): Machine[T] = 
    new Machine:
      val mm = MMConstruction(Context[T *: EmptyTuple]).compile(regex)
      def run(str: String): Option[T] = mm.parse(EmptyTuple, str.toCharArray().toList).map(_.head)
