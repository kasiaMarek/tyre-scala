import org.scalatest.funsuite.AnyFunSuite

class MachineTest extends AnyFunSuite:

	test("Simple Pred"):
		val tyre = Pred(_ == 'A')
		val m = tyre.compile()
		assert(m.run("A").contains('A'))
		assert(m.run("X").isEmpty)

	test("Simple And"):
		val tyre = And(Pred(_ == 'A'), Pred(_ == 'B'))
		val m = tyre.compile()
		assert(m.run("AB").contains(('A', 'B')))
		assert(m.run("BA").isEmpty)

	test("Simple Or"):
		val tyre = Or(Pred(_ == 'A'), Pred(_ == 'B'))
		val m = tyre.compile()
		assert(m.run("A").contains(Left('A')))
		assert(m.run("B").contains(Right('B')))
		assert(m.run("X").isEmpty)

	test("Simple Star"):
		val tyre = Star(Pred(_ == 'A'))
		val m = tyre.compile()
		assert(m.run("A").contains(List('A')))
		assert(m.run("AA").contains(List('A', 'A')))
		assert(m.run("X").isEmpty)

	test("Simple Epsilon"):
		val tyre = Epsilon
		val m = tyre.compile()
		assert(m.run("").contains(()))
		assert(m.run("X").isEmpty)
