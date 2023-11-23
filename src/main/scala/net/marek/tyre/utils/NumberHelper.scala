package net.marek.tyre.utils

object NumberHelper:

  extension (n: Int)
    def pow(p: Int): Int = p match
      case x if x > 0 => n * n.pow(x - 1)
      case _ => 1

  def digit(c: Char): Int = c - '0'
  private def at(c: Char, pos: Int, base: Int): Int = digit(c) * base.pow(pos)
  private def number(base: Int, cs: Seq[Char]): Int = cs.reverse.zipWithIndex.map(at(_, _, base)).sum

  private val hexRanges: Set[Range] = Set(Range('0', '9'), Range('A', 'F'), Range('a', 'f'))
  def isHex(c: Char): Boolean = hexRanges.exists(_.contains(c))
  def hex(cs: Char*): Int = number(16, cs)
  def decimal(cs: Char*): Int = number(10, cs)
