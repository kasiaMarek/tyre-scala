package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import Pred.given

class AutomatonTest extends AnyFunSuite:

	test("Simple Pred"):
		val tyre = 'A'
		val m = tyre.compile()
		assert(m.run("A").contains('A'))
		assert(m.run("X").isEmpty)

	test("Simple And"):
		val tyre = 'A' <*> 'B'
		val m = tyre.compile()
		assert(m.run("AB").contains(('A', 'B')))
		assert(m.run("BA").isEmpty)

	test("Simple Or"):
		val tyre = 'A' <|> 'B'
		val m = tyre.compile()
		assert(m.run("A").contains(Left('A')))
		assert(m.run("B").contains(Right('B')))
		assert(m.run("X").isEmpty)

	test("Simple Star"):
		val tyre = 'A'.rep
		val m = tyre.compile()
		assert(m.run("A").contains(List('A')))
		assert(m.run("AA").contains(List('A', 'A')))
		assert(m.run("").contains(Nil))
		assert(m.run("X").isEmpty)

	test("Simple Epsilon"):
		val tyre = Epsilon
		val m = tyre.compile()
		assert(m.run("").contains(()))
		assert(m.run("X").isEmpty)

	test("Advanced Star"):
		val tyre = (('A' <*> 'B').rep <|> ('X' <*> 'Y').rep <*> 'Q').rep
		val m = tyre.compile()
		val abq = (Left(List(('A', 'B'),('A', 'B'))), 'Q')
		val xyq = (Right(List(('X', 'Y'))), 'Q')
		assert(m.run("ABABQABABQ").contains(List(abq, abq)))
		assert(m.run("XYQXYQ").contains(List(xyq, xyq)))
		assert(m.run("XYQABABQ").contains(List(xyq, abq)))
		assert(m.run("XYABABQ").isEmpty)
		assert(m.run("").contains(Nil))

	test("Star in Star"):
		val tyre = ('A').rep.rep
		val m = tyre.compile()
		assert(m.run("AAA").contains(List(List('A', 'A', 'A'))))
		assert(m.run("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA").isDefined)
		assert(m.run("AAX").isEmpty)
