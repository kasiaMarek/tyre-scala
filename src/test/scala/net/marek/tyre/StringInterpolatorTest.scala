package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.unused

class StringInterpolatorTest extends AnyFunSuite:

  test("Tyre construction"):
    assertCompiles("""tyre"x"""")
    assertDoesNotCompile("""tyre"x|*"""")
    @unused val te: Tyre[Char] = tyre"a|b"
    @unused val tes: Tyre[Either[Char, Char]] = tyre"a||b"
    @unused val tt: Tyre[(Char, Char, Char, Char, Char)] = tyre"abcde"
    val tm = tyre"a|b".map(_ => 'o')
    @unused val t: Tyre[Either[Char, (Char, Char, Char)]] = tyre"${tm}|lpk"

  test("Tyre execution"):
    val t = tyre"a?"
    val m = t.compile()
    assertResult(Some(Some('a')))(m.run("a"))
    assertResult(Some(None))(m.run(""))
    assertResult(None)(m.run("aa"))
