package net.marek.tyre

import net.marek.tyre.diagnostic.Renderer

trait Automaton[T]:
  def run(str: String): Option[T]
  def getAll(str: String): List[T]
  def show(renderer: Renderer, cs: Set[Char]): String

extension [T](regex: Tyre[T])
  def compile(): Automaton[T] =
    new Automaton:
      val mm = Compiler(Context[T *: EmptyTuple]).compile(regex)
      def run(str: String): Option[T] = mm.parse(EmptyTuple, str.toCharArray().toList).map(_.head)
      def getAll(str: String) = mm.parseAll(EmptyTuple, str.toCharArray().toList).map(_.head)
      def show(renderer: Renderer, cs: Set[Char]): String = mm.show(renderer, cs)
