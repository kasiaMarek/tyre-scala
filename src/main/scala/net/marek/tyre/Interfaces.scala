package net.marek.tyre

trait Machine[T]:
  def run(str: String): Option[T]
  def getAll(str: String): List[T]
  def show(cs: Set[Char]): String

extension [T](regex: Tyre[T])
  def compile(): Machine[T] =
    new Machine:
      val mm = MMConstruction(Context[T *: EmptyTuple]).compile(regex)
      def run(str: String): Option[T] = mm.parse(EmptyTuple, str.toCharArray().toList).map(_.head)
      def getAll(str: String) = mm.parseAll(EmptyTuple, str.toCharArray().toList).map(_.head)
      def show(cs: Set[Char]): String = mm.show(cs)
