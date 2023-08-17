import org.scalatest.funsuite.AnyFunSuite

class StringParserTest extends AnyFunSuite:

	test("Simple parser"):
		println(TyreParser(""))
		println(TyreParser("x"))
		println(TyreParser("(x)"))
		println(TyreParser("xy"))
		println(TyreParser("x|y"))
		println(TyreParser("(x|y)"))
		println(TyreParser("x*"))
		println(TyreParser("(x(y|a))b"))
		println(TyreParser("x(y|a)b"))
		println(TyreParser("x|y*"))
		println(TyreParser("x)y"))
		println(TyreParser("x|*"))
		println(TyreParser("(x*y)*"))
		assertCompiles("""tyre"x"""")
		assertDoesNotCompile("""tyre"x|*"""")
		val t = tyre"x|(xh)"
		val m: "x" = "x"
		val h = tyre"$m"
		println(t)
		// val good = tyre"x"
		// val bad = tyre"(aa"