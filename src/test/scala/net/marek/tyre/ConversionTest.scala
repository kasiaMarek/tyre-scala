package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.unused

class ConversionTest extends AnyFunSuite:

  test("Singleton"):
    @unused val _: Tyre[('a', List[Char])] = tyre"a.*"
    @unused val _: Tyre[('a', List[Char])] = tyre"[a]!l.*"
    @unused val _: Tyre[('a' | 'b', List[Char])] = tyre"[ab]!l.*"
    @unused val _: Tyre[('a' | 'b' | 'c', List[Char])] = tyre"[a-c]!l.*"
    @unused val _: Tyre[(List['a' | 'b' | 'c'], List[Char])] = tyre"[a-c]!l*.*"
    val g = 8
    //assertDoesNotCompile(tyre"[a-\u3333]!l.*")  // to many literal types in union


