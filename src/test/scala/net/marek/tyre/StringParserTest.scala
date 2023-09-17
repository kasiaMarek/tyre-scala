package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite
import java.time.LocalTime
import scala.annotation.unused
import Re.char

class StringParserTest extends AnyFunSuite:

  def assertDoesNotParse(pattern: String) =
    assert(TyreParser(tokenize(pattern)).isEmpty)

  def assertParses(pattern: String, result: Re) =
    assertResult(Some(result), pattern)(TyreParser(tokenize(pattern)))

  test("Simple parser"):
    assertDoesNotParse("")
    assertParses("x", char('x'))
    assertParses("(x)", char('x'))
    assertParses("xy", ReAnd(char('x'), char('y')))
    assertParses("x|y", ReOr(char('x'), char('y')))
    assertParses("x||y", ReOrS(char('x'), char('y')))
    assertParses("(x|y)", ReOr(char('x'), char('y')))
    assertParses("x*", ReStar(char('x')))
    assertParses("x?", ReOpt(char('x')))
    assertParses("(x(y|a))b", ReAnd(ReAnd(char('x'), ReOr(char('y'), char('a'))), char('b')))
    assertParses("xy|a", ReOr(ReAnd(char('x'), char('y')), char('a')))
    assertParses("x|ya", ReOr(char('x'), ReAnd(char('y'), char('a'))))
    assertParses("x(y|a)b", ReAnd(char('x'), ReAnd(ReOr(char('y'), char('a')), char('b'))))
    assertParses("x(y||a)b", ReAnd(char('x'), ReAnd(ReOrS(char('y'), char('a')), char('b'))))
    assertParses("x|y*", ReOr(char('x'), ReStar(char('y'))))
    assertParses("(x*y)*", ReStar(ReAnd(ReStar(char('x')), char('y'))))
    assertParses("x\\)", ReAnd(char('x'), char(')')))
    assertParses("x*\\*y\\\\", ReAnd(ReStar(char('x')), ReAnd(char('*'), ReAnd(char('y'), char('\\')))))
    assertParses("[s-v]", ReOneOf(List('s', 't', 'u', 'v')))
    assertParses("@|l", ReOr(ReHole(0), char('l')))
    assertDoesNotParse("x)y")
    assertDoesNotParse("x|*")

  test("Tyre construction"):
    assertCompiles("""tyre"x"""")
    assertDoesNotCompile("""tyre"x|*"""")
    @unused val te: Tyre[Char] = tyre"a|b"
    @unused val tes: Tyre[Either[Char, Char]] = tyre"a||b"
    val tm = tyre"a|b".map(_ => 'o')
    @unused val t: Tyre[Either[Char, (Char, Char, Char)]] = tyre"${tm}|lpk"
    val to = tyre"a?"
    val m = to.compile()
    assertResult(Some(Some('a')))(m.run("a"))
    assertResult(Some(None))(m.run(""))
    assertResult(None)(m.run("aa"))

  test("Time parser"):
    def digit(c: Char): Int = c - '0'
    def decimal(c: Char, p: Int): Int = digit(c)*10.pow(p)
    def number(cs: Char*): Int =
      cs.reverse.zipWithIndex.map(decimal(_, _)).sum
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

  def tokenize(str: String): List[Token] =
    str.toList.zipWithIndex.map {
      case ('@', idx) => Hole(idx)
      case (e, _) => e
    }

  extension (n: Int)
    def pow(p: Int): Int = p match
      case x if x > 0 => n * n.pow(x-1)
      case _ => 1
