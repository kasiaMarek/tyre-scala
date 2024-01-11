package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.unused

class ConversionTest extends AnyFunSuite:

  test("Singleton types"):
    @unused val _: Tyre[('a', List[Char])] = tyre"a.*"
    @unused val _: Tyre[('a', List[Char])] = tyre"[a]!l.*"
    @unused val _: Tyre[('a' | 'b', List[Char])] = tyre"[ab]!l.*"
    @unused val _: Tyre[('a' | 'b' | 'c', List[Char])] = tyre"[a-c]!l.*"
    @unused val _: Tyre[(List['a' | 'b' | 'c' | 'w'], List[Char])] = tyre"[a-cw]!l*.*"
    assertDoesNotCompile("""tyre"[a-\u3333]!l.*"""")  // to many literal types in union

  test("Singleton regex"):
    val t = tyre"[a-c]!l.*q"
    val m = t.compile()
    val res: Option[('a' | 'b' | 'c', List[Char], 'q')] = m.run("bxxq")
    assertResult(Some(('b', List('x', 'x'), 'q')))(res)
