package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalTime

class RegexTest extends AnyFunSuite:

  test("Time parser"):
    val ht = tyre"[0-1][0-9]|2[0-3]".map(number(_, _))
    val mt = tyre"[0-5][0-9]".map(number(_, _))
    val tt = tyre"$ht:$mt".map: t =>
      val (h, _, m) = t
      LocalTime.of(h, m)
    val tm = tt.compile()
    val time = tm.run("21:25")
    assertResult(Some(LocalTime.of(21, 25)))(time)
    assertResult(None)(tm.run("52:12"))
    assertResult(None)(tm.run("22:"))
    assertResult(None)(tm.run("22:222"))
    assertResult(None)(tm.run("x"))

  def digit(c: Char): Int = c - '0'
  def decimal(c: Char, p: Int): Int = digit(c)*10.pow(p)
  def number(cs: Char*): Int =
      cs.reverse.zipWithIndex.map(decimal(_, _)).sum

  extension (n: Int)
    def pow(p: Int): Int = p match
      case x if x > 0 => n * n.pow(x-1)
      case _ => 1
