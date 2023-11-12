package net.marek.tyre

object NumberHelper:

  extension (n: Int)
    def pow(p: Int): Int = p match
      case x if x > 0 => n * n.pow(x-1)
      case _ => 1

  def digit(c: Char): Int = c - '0'

  def isHex(c: Char): Boolean = c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'
  private def hexAt(c: Char, pos: Int): Int = digit(c) * 16.pow(pos)
  def hex(cs: Char*): Int = cs.reverse.zipWithIndex.map(hexAt(_, _)).sum

  private def decimalAt(c: Char, pos: Int): Int = digit(c)*10.pow(pos)
  def decimal(cs: Char*): Int = cs.reverse.zipWithIndex.map(decimalAt(_, _)).sum
