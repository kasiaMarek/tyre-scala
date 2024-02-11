package net.marek.tyre.benchmark

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import net.marek.tyre.*
import scala.util.Random

@State(Scope.Thread)
class RegexBenchmark:

  @Benchmark
  def runTimeTyre: Unit = timeData.foreach(tyreTimeParser.run)

  @Benchmark
  def runTimeRegex: Unit = timeData.foreach:
    case regexTimeParser(h, m) => (h.toInt, m.toInt)

  @Benchmark
  def runEmailTyre10: Unit = emailData10.foreach(tyreEmailParser.run)

  @Benchmark
  def runEmailRegex10: Unit = emailData10.foreach:
    case regexEmailParser(u, d) => (u, d)

  @Benchmark
  def runEmailTyre100: Unit = emailData100.foreach(tyreEmailParser.run)

  private val tyreTimeParser =
    val ht = tyre"[0-1]\d|2[0-3]"
    val mt = tyre"[0-5]\d"
    val tt = tyre"$ht:$mt".map: t =>
      val ((h10, h1), _, m10, m1) = t
      (h10.toInt * 10 + h1.toInt, m10.toInt * 10 + m1.toInt)
    tt.compile()

  private val regexTimeParser = """([0-1]\d|2[0-3]):([0-5]\d)""".r

  private val tyreEmailParser =
    val ut = tyre".+!s"
    val dt = tyre"(.+\..+)!s"
    val et = tyre"$ut@$dt"
    et.compile()

  private val regexEmailParser = """(.+)@(.+\..+)""".r

  private val rand = Random()

  private def drawTime =
    val h = rand.between(0, 24)
    val m = rand.between(0, 60)
    f"$h%02d:$m%02d"
  private val timeData = List.tabulate(10_000)(_ => drawTime)

  private def drawLetter = rand.between('a', 'z' + 1).toChar
  private def drawEmail(length: Int) =
    val u = List.tabulate(length)(_ => drawLetter).toString
    val d = List.tabulate(length)(_ => drawLetter).toString
    s"$u@$d.com"
  private val emailData10 = List.tabulate(10_000)(_ => drawEmail(10))
  private val emailData100 = List.tabulate(10_000)(_ => drawEmail(100))
