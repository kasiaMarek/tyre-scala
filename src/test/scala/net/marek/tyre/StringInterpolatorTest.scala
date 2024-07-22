package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.unused

class StringInterpolatorTest extends AnyFunSuite:

  test("Tyre construction"):
    assertCompiles("""tyre"x"""")
    assertDoesNotCompile("""tyre"x|*"""")
    @unused val _: Tyre[Char] = tyre"a|b"
    @unused val _: Tyre[Either[Char, Char]] = tyre"a||b"
    @unused val _: Tyre[(Char, Char, Char, Char, Char)] = tyre"abcde"
    val tm = tyre"a|b".map(_ => 'o')
    @unused val _: Tyre[Either[Char, (Char, Char, Char)]] = tyre"${tm}||lpk"
    @unused val _: Tyre[Char | (Char, Char, Char)] = tyre"${tm}|lpk"

  test("Tyre escape sequence"):
    val tb: Tyre[(Char,Char)] = tyre"x\\"
    val mb = tb.compile()
    assertResult(Some(('x', '\\')))(mb.run("x\\"))
    val td: Tyre[(Char,Char)] = tyre"\-x"
    val md = td.compile()
    assertResult(Some(('-', 'x')))(md.run("-x"))
    val tq: Tyre[(Char,Char)] = tyre"\"x"
    val mq = tq.compile()
    assertResult(Some(('"', 'x')))(mq.run("\"x"))
    val ts: Tyre[(Char,Char)] = tyre"$$x"
    val ms = ts.compile()
    assertResult(Some(('$', 'x')))(ms.run("$x"))

  test("Tyre execution"):
    val t = tyre"a?"
    val m = t.compile()
    assertResult(Some(Some('a')))(m.run("a"))
    assertResult(Some(None))(m.run(""))
    assertResult(None)(m.run("aa"))

  test("generic"):
    val _ = [T, B] => (aa : Tyre[T], bb: Tyre[B]) => tyre"$aa$bb"
    val _ = [T, B] => (aa : Conv[Int *: EmptyTuple, T], bb: Tyre[B]) => tyre"$bb$aa"
