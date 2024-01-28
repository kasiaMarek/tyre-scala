package net.marek.tyre.benchmark

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import net.marek.tyre.*
import scala.util.Random

@State(Scope.Thread)
class RegexBenchmark:

  @Benchmark
  def runTyre: Unit = data.foreach(tyreTimeParser.run)

  @Benchmark
  def runRegex: Unit = data.foreach:
    case regexTimeParser(h, m) => (h.toInt, m.toInt)

  private val tyreTimeParser =
    val ht = tyre"[0-1]\d|2[0-3]"
    val mt = tyre"[0-5]\d"
    val tt = tyre"$ht:$mt".map: t =>
      val ((h10,h1), _, m10, m1) = t
      (h10.toInt * 10 + h1.toInt, m10.toInt * 10 + m1.toInt)
    tt.compile()

  private val regexTimeParser = """([0-1]\d|2[0-3]):([0-5]\d)""".r

  private val rand = Random()
  private def drawTime =
    val h = rand.between(0,24)
    val m = rand.between(0,60)
    f"$h%02d:$m%02d"
  private val data = List.tabulate(100_000)(_ => drawTime)
