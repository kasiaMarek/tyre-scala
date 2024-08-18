package net.marek.tyre.benchmark

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import net.marek.tyre.*
import scala.util.Random

@State(Scope.Thread)
class RegexBenchmark:

  @Benchmark
  def runTimeRegex: Unit = timeData.foreach:
    case regexTimeParser(h, m) => (h.toInt, m.toInt)

  @Benchmark
  def runTimeTyre: Unit = timeData.foreach(tyreTimeParser.run)

  @Benchmark
  def runEmailRegex15: Unit = emailData15.foreach:
    case regexEmailParser(u, d) => (u, d)

  @Benchmark
  def runEmailTyre15: Unit = emailData15.foreach(tyreEmailParser.run)

  @Benchmark
  def runEmailTyre45: Unit = emailData45.foreach(tyreEmailParser.run)

  @Benchmark
  def runEmailTyre75: Unit = emailData75.foreach(tyreEmailParser.run)

  @Benchmark
  def runEmailTyre105: Unit = emailData105.foreach(tyreEmailParser.run)

  @Benchmark
  def runEmailTyre135: Unit = emailData135.foreach(tyreEmailParser.run)

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
  private val timeData = List.tabulate(1_000)(_ => drawTime)

  private def drawLetter = rand.between('a', 'z' + 1).toChar
  private def drawEmail(length: Int) =
    val u = List.tabulate(length)(_ => drawLetter).toString
    val d = List.tabulate(length)(_ => drawLetter).toString
    s"$u@$d.com"
  // the total lenght is 2*lenght+5 -> user, domain and constant part
  private val emailData15 = List.tabulate(1_000)(_ => drawEmail(5))
  private val emailData45 = List.tabulate(1_000)(_ => drawEmail(20))
  private val emailData75 = List.tabulate(1_000)(_ => drawEmail(35))
  private val emailData105 = List.tabulate(1_000)(_ => drawEmail(50))
  private val emailData135 = List.tabulate(1_000)(_ => drawEmail(65))
