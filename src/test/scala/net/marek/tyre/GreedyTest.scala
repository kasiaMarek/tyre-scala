package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

class GreedyTest extends AnyFunSuite:

  test("Star and option are greedy"):
    val ts = tyre"a?a?"
    val ms = ts.compile()
    assert(ms.run("a").contains((Some('a'), None)))
    val t = tyre"ab|[a-z]c"
    val tc1 = tyre"$t*$t?"
    val mc1 = tc1.compile()
    assert(mc1.run("abab").contains((List(('a', 'b'), ('a', 'b')), None)))
    val tc2 = tyre"$t*$t*"
    val mc2 = tc2.compile()
    assert(mc2.run("abab").contains((List(('a', 'b'), ('a', 'b')), Nil)))
    val tc3 = tyre"$t?$t*"
    val mc3 = tc3.compile()
    assert(mc3.run("abab").contains((Some(('a', 'b')), List(('a', 'b')))))
