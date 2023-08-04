import org.scalatest.funsuite.AnyFunSuite

class StringParserTest extends AnyFunSuite:

	test("Simple parser"):
		println(TyreParser(""))
		println(TyreParser("x"))
		println(TyreParser("xy|ab"))
		println(TyreParser("x(y|a)b"))
		println(TyreParser("x|y*"))
		println(TyreParser("x)y"))
		println(TyreParser("x|*"))
		println(TyreParser("(x*y)*"))
		val t = tyre"x"
		println(t)
		// val good = tyre"x"
		// val bad = tyre"(aa"