import org.scalatest.funsuite.AnyFunSuite
import Re.char

class StringParserTest extends AnyFunSuite:

  def assertDoesNotParse(pattern: String) =
    assert(TyreParser(pattern).isEmpty)

  def assertParses(pattern: String, result: Re) =
    assertResult(Some(result), pattern)(TyreParser(pattern))

  test("Simple parser"):
    assertDoesNotParse("")
    assertParses("x", char('x'))
    assertParses("(x)", char('x'))
    assertParses("xy", ReAnd(char('x'), char('y')))
    assertParses("x|y", ReOr(char('x'), char('y')))
    assertParses("(x|y)", ReOr(char('x'), char('y')))
    assertParses("x*", ReStar(char('x')))
    assertParses("x?", ReOr(char('x'), ReEpsilon))
    assertParses("(x(y|a))b", ReAnd(ReAnd(char('x'), ReOr(char('y'), char('a'))), char('b')))
    assertParses("xy|a", ReOr(ReAnd(char('x'), char('y')), char('a')))
    assertParses("x|ya", ReOr(char('x'), ReAnd(char('y'), char('a'))))
    assertParses("x(y|a)b", ReAnd(char('x'), ReAnd(ReOr(char('y'), char('a')), char('b'))))
    assertParses("x|y*", ReOr(char('x'), ReStar(char('y'))))
    assertParses("(x*y)*", ReStar(ReAnd(ReStar(char('x')), char('y'))))
    assertParses("x\\)", ReAnd(char('x'), char(')')))
    assertParses("x*\\*y\\\\", ReAnd(ReStar(char('x')), ReAnd(char('*'), ReAnd(char('y'), char('\\')))))
    assertParses("[s-v]", ReOneOf(List('s', 't', 'u', 'v')))
    assertDoesNotParse("x)y")
    assertDoesNotParse("x|*")
    assertCompiles("""tyre"x"""")
    assertDoesNotCompile("""tyre"x|*"""")
