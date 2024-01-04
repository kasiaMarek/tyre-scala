package net.marek.tyre.utils

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.ToExpr

private[tyre] case class Range(from: Char, to: Char):
  def contains(c: Char): Boolean = c >= from && c <= to
  def getChars: List[Char] = (from to to).toList
  def size = to - from + 1

private[tyre] object Range:
  def apply(char: Char): Range = Range(char, char)

  given Conversion[Char, Range] = Range(_)

  given ToExpr[Range] with
    def apply(r: Range)(using Quotes) = r match
      case Range(from, to) => '{ Range(${ Expr(from) }, ${ Expr(to) }) }
