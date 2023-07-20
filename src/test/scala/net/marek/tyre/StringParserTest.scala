import org.scalatest.funsuite.AnyFunSuite

class StringParserTest extends AnyFunSuite:

	test("Simple parser"):
		println(TyreParser("x"))
		println(TyreParser("x)"))
		println(TyreParser(")"))
		// val good = tyre"x"
		// val bad = tyre"(aa"