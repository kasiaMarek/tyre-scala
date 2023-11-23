package net.marek.tyre.diagnostic

import scala.collection.mutable

private[tyre] trait Renderer:
  def add(initState: Any): Unit
  def add(state: Any, nextState: Any, cs: Set[Char]): Unit
  def render: String

private[tyre] class GraphvizRenderer extends Renderer:
  val sb = mutable.StringBuilder()
  def add(initState: Any): Unit =
    sb.append(s"start -> $initState\n".replaceAll("[\\.\\$@]", "_"))
  def add(state: Any, nextState: Any, cs: Set[Char]): Unit =
    sb.append(s"$state -> $nextState".replaceAll("[\\.\\$@]", "_"))
    sb.append(s" [label = \"${cs.mkString(", ")}\" ];\n")
  def render: String = sb.toString
