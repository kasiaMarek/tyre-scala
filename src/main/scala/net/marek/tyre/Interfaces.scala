package net.marek.tyre

import net.marek.tyre.automaton.TyreCompiler
import net.marek.tyre.diagnostic.Renderer

export net.marek.tyre.pattern.tyre

trait Automaton[T]:
  def run(str: String): Option[T]
  private[tyre] def getAll(str: String): List[T]
  private[tyre] def show(renderer: Renderer, cs: Set[Char]): String

extension [T](regex: Tyre[T])
  def compile(): Automaton[T] =
    new Automaton:
      private val mm = TyreCompiler[T].compile(regex)
      def run(str: String): Option[T] = mm.parse(EmptyTuple, str.toCharArray().toList).map(_.head)
      private[tyre] def getAll(str: String) = mm.parseAll(EmptyTuple, str.toCharArray().toList).map(_.head)
      private[tyre] def show(renderer: Renderer, cs: Set[Char]): String = mm.show(renderer, cs)
