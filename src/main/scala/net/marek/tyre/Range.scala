package net.marek.tyre

import scala.quoted.{Expr, Quotes, ToExpr}

case class Range(from: Char, to: Char)

object Range:
  def apply(char: Char): Range = Range(char, char)

given ToExpr[Range] with
  def apply(r: Range)(using Quotes) = r match
    case Range(from, to) => '{Range( ${Expr(from)}, ${Expr(to)})}
