package net.marek.tyre

import org.scalatest.funsuite.AnyFunSuite

import java.time.LocalTime

class RegexTest extends AnyFunSuite:

  test("Double star"):
    val t = tyre"[A-Za-z0-9_]"
    val tt = tyre"($t$t*)*".map(string) // doesn't make really sense, but covers a corner-case
    val tm = tt.compile()
    val result = tm.run("abbabb")
    assertResult(Some("abbabb"))(result)
    assertResult(None)(tm.run("-"))

  test("Time parser"):
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

  test("Email parser"):
    // Dash escaping doesn't work
    // val ldudt = tyre"[A-Za-z0-9_\\-]"
    val lddut = tyre"[A-Za-z0-9_]"
    // val lddt = tyre"[A-Za-z0-9\\-]"
    val lddt = tyre"[A-Za-z0-9]"
    val ldt = tyre"[A-Za-z0-9]"
    val lt = tyre"[A-Za-z]"
    val ut = tyre"${lddut}${lddut}*(.${lddut}${lddut}*)*".map(string) // user (local) part
    val sdt = tyre"${ldt}(${lddt}*${ldt})?".map(string) // subdomain element
    val tdt = tyre"$lt$lt$lt*".map(string)  // top domain
    val dt = tyre"$sdt.($sdt.)*$tdt".map(string)  // domain part
    val et = tyre"$ut@$dt"  // email
    val tm = et.compile()
    val email = tm.run("robert.marek@fingo.net")
    assert(email.isDefined)
    val (user, _, domain) = email.get
    assertResult("robert.marek")(user)
    assertResult("fingo.net")(domain)
    assertResult(None)(tm.run("robert.marek"))
    assertResult(None)(tm.run("@fingo.net"))
    assertResult(None)(tm.run("robert@marek"))
    assertResult(None)(tm.run(".robert@fingo.net"))

  test("Money parser"):
    // type Symbol = '$' | '€' |'£' | '₣' | '₿'
    type Symbol = Char
    case class Money(amount: BigDecimal, currency: Symbol)
    val pt = tyre"[$$€£₣₿] ?[1-9][0-9]*(.[0-9][0-9])?"
    val st = tyre"[1-9][0-9]*(.[0-9][0-9])? ?[$$€£₣₿]"
    val mt = tyre"$pt|$st".map: e =>
      val t = e.fold(
        l => (l(0), l(2) :: l(3), l(4)),
        r => (r(4), r(0) :: r(1), r(2))
      )
      val integral = number(t(1)*)
      val fraction = t(2).map(p => (number(p(1), p(2))))
      (t(0), bigDecimal(integral, fraction, 2))
    val tm = mt.compile()
    val amount = tm.run("€1643.52").map(t => Money(t(1), t(0)))
    assertResult(Some(Money(BigDecimal(1643.52), '€')))(amount)

  def digit(c: Char): Int = c - '0'
  def decimal(c: Char, p: Int): Int =
    digit(c)*10.pow(p)
  def number(cs: Char*): Int =
      cs.reverse.zipWithIndex.map(decimal(_, _)).sum
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

  extension (n: Int)
    def pow(p: Int): Int = p match
      case x if x > 0 => n * n.pow(x-1)
      case _ => 1
