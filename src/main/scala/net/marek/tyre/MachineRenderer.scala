package net.marek.tyre

import scala.collection.mutable

trait MachineRenderer:
  def add(initState: Any): Unit
  def add(state: Any, nextState: Any, cs: Set[Char]): Unit
  def render: String

class GraphvizMachineRenderer extends MachineRenderer:
  val sb = mutable.StringBuilder()
  def add(initState: Any): Unit =
    sb.append(s"start -> $initState\n".replaceAll("[\\.\\$@]", "_"))
  def add(state: Any, nextState: Any, cs: Set[Char]): Unit =
    sb.append(s"$state -> $nextState".replaceAll("[\\.\\$@]", "_"))
    sb.append(s" [label = \"${cs.mkString(", ")}\" ];\n")
  def render: String = sb.toString
