package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite
import java.time.LocalTime
import NumberHelper.*

class RegexTest extends AnyFunSuite:

  test("Double star"):
    val tt = tyre"(\w\w*)*".map(string) // doesn't make really sense, but covers a corner-case
    val tm = tt.compile()
    val result = tm.run("abbabb")
    assertResult(Some("abbabb"))(result)
    assertResult(None)(tm.run("-"))

  test("Time parser"):
    val ht = tyre"[0-1]\d|2[0-3]".map(decimal(_, _))
    val mt = tyre"[0-5]\d".map(decimal(_, _))
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

  test("Email parser"):
    val lddut = tyre"[\w\-]"
    val lddt = tyre"[A-Za-z0-9\-]"
    val ldt = tyre"[A-Za-z0-9]"
    val lt = tyre"[A-Za-z]"
    val ut = tyre"(${lddut}+(\.${lddut}+)*)!s" // user (local) part
    val sdt = tyre"${ldt}(${lddt}*${ldt})?" // subdomain element
    val tdt = tyre"$lt$lt+" // top domain
    val dt = tyre"($sdt\.($sdt\.)*$tdt)!s"  // domain part
    val et = tyre"$ut@$dt"  // email
    val tm = et.compile()
    val email = tm.run("some.user-name@example.com")
    assert(email.isDefined)
    val (user, _, domain) = email.get
    assertResult("some.user-name")(user)
    assertResult("example.com")(domain)
    assertResult(None)(tm.run("some.user-name"))
    assertResult(None)(tm.run("@example.com"))
    assertResult(None)(tm.run("some@user-name"))
    assertResult(None)(tm.run(".user-namet@example.com"))

  test("Money parser"):
    type Symbol = Char
    case class Money(amount: BigDecimal, currency: Symbol)
    val pt = tyre"[$$€£₣₿]\h?[1-9]\d*(\.\d\d)?"
    val st = tyre"[1-9]\d*(\.\d\d)? ?[$$€£₣₿]"
    val mt = tyre"$pt||$st".map: e =>
      val t = e.fold(
        l => (l(0), l(2) :: l(3), l(4)),
        r => (r(4), r(0) :: r(1), r(2))
      )
      val integral = decimal(t(1)*)
      val fraction = t(2).map(p => (decimal(p(1), p(2))))
      (t(0), bigDecimal(integral, fraction, 2))
    val tm = mt.compile()
    val amount = tm.run("€ 1643.52").map(t => Money(t(1), t(0)))
    assertResult(Some(Money(BigDecimal(1643.52), '€')))(amount)

  test("Google map coordinates parser"):
    val lgdt = tyre"(1[0-7]|\d)?\d".map(int)
    val ltdt = tyre"[1-8]?\d".map(int)
    val mt = tyre"[0-5]\d".map(int)
    val st = tyre"[0-5]\d\.\d".map(double)
    val ct = tyre"$lgdt°$mt'$st\"[NS]\h$ltdt°$mt'$st\"[WE]"
    val cm = ct.compile()
    val data = """51°06'36.3"N 17°01'55.7"E"""
    val coordinates = cm.run(data)
    assert(coordinates.isDefined)
    val (lgDeg, _, lgMin, _, lgSec, _, lgDir, _, ltDeg, _, ltMin, _, ltSec, _, ltDir) = coordinates.get
    assertResult(51)(lgDeg)
    assertResult(36.3)(lgSec)
    assertResult('N')(lgDir)
    assertResult(17)(ltDeg)
    assertResult(1)(ltMin)
    assertResult(None)(cm.run("""51°06'36.3" 17°01'55.7"E"""))
    assertResult(None)(cm.run("""51°06'36.3"N"""))
    assertResult(None)(cm.run("foo"))

  def bigDecimal(int: Int, fract: Option[Int], scale: Int): BigDecimal =
    val mult = 10.pow(scale)
    ((BigDecimal(int) * BigDecimal(mult) + BigDecimal(fract.getOrElse(0))) / BigDecimal(mult)).setScale(scale)
  def string(x: Any): String = x match
    case h *: t => s"${string(h)}${string(t)}"
    case h :: t => s"${string(h)}${string(t)}"
    case Some(x) => s"${string(x)}"
    case Left(x) => s"${string(x)}"
    case Right(x) => s"${string(x)}"
    case EmptyTuple => ""
    case Nil => ""
    case None => ""
    case s => s.toString
  def int(x: Any): Int = string(x).toInt
  def double(x: Any): Double = string(x).toDouble


