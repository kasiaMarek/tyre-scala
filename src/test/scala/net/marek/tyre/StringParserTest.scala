package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import Re.char

class StringParserTest extends AnyFunSuite:

  inline def assertDoesNotParse(pattern: String) =
    assert(TyreParser(tokenize(pattern)).isEmpty)

  inline def assertParses(pattern: String, result: Re) =
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
    assertParses("xy+", ReAnd(char('x'), RePlus(char('y'))))
    assertParses("(xy)+", RePlus(ReAnd(char('x'), char('y'))))
    assertParses("x\\+", ReAnd(char('x'), char('+')))
    assertParses("x\\)", ReAnd(char('x'), char(')')))
    assertParses("x*\\*y\\\\", ReAnd(ReStar(char('x')), ReAnd(char('*'), ReAnd(char('y'), char('\\')))))
    assertParses("[s-v]", ReIn(List(Range('s', 'v'))))
    assertParses("[abs-vz]", ReIn(List(Range('a'), Range('b'), Range('s', 'v'), Range('z'))))
    assertParses("[^s-v]", ReNotIn(List(Range('s', 'v'))))
    assertParses("[.]", ReIn(List(Range('.', '.'))))
    assertParses("@|l", ReOr(ReHole(0), char('l')))
    assertParses("s\\tt", ReAnd(char('s'), ReAnd(char('\t'), char('t'))))
    assertParses("[\\sa]", ReIn(List(' ', '\t', '\n', '\r', '\f', '\u000B', 'a')))
    assertParses(
      "\\w\\s\\D",
      ReAnd(
        ReIn(List('_', Range('a', 'z'), Range('A', 'Z'), Range('0', '9'))),
        ReAnd(
          ReIn(List(' ', '\t', '\n', '\r', '\f', '\u000B')),
          ReNotIn(List(Range('0', '9')))
        )
      )
    )
    assertParses("x.\\.", ReAnd(char('x'), ReAnd(ReAny, char('.'))))
    assertParses("(abc)!sx", ReAnd(ReCast(ReAnd(char('a'), ReAnd(char('b'), char('c'))), CastOp.Stringify), char('x')))
    assertDoesNotParse("x)y")
    assertDoesNotParse("x|*")

  def tokenize(str: String): List[Token] =
    str.toList.zipWithIndex.map {
      case ('@', idx) => Hole(idx)
      case (e, _) => e
    }
