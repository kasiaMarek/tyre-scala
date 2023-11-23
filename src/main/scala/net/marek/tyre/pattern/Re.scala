package net.marek.tyre.pattern

import net.marek.tyre.utils.Range

// Re - untyped regular expression, used to buuld expressions from string representations
sealed private trait Re

private case object ReAny extends Re:
  override def toString(): String = "."

private case class ReIn(cs: List[Range]) extends Re:
  override def toString(): String = cs
    .map:
      case Range(x, y) if x == y => x.toString()
      case Range(x, y) => s"$x-$y"
    .mkString("[", "", "]")

private case class ReNotIn(cs: List[Range]) extends Re:
  override def toString(): String = cs
    .map:
      case Range(x, y) if x == y => x.toString()
      case Range(x, y) => s"$x-$y"
    .mkString("[^", "", "]")

private case class ReOr(left: Re, right: Re) extends Re:
  override def toString(): String = s"($left|$right)"

private case class ReOrS(left: Re, right: Re) extends Re:
  override def toString(): String = s"($left||$right)"

private case class ReAnd(left: Re, right: Re) extends Re:
  override def toString(): String = s"($left$right)"

private case class ReStar(re: Re) extends Re:
  override def toString(): String = s"$re*"

private case class RePlus(re: Re) extends Re:
  override def toString(): String = s"$re+"

private case class ReOpt(re: Re) extends Re:
  override def toString(): String = s"$re?"

private case class ReCast(re: Re, conv: CastOp) extends Re:
  override def toString(): String = s"$re!$conv.symbol"

private case object ReEpsilon extends Re:
  override def toString(): String = "_"

private case class ReHole(index: Int) extends Re:
  override def toString(): String = s"@$index"

private object Re:
  def char(c: Char): ReIn = ReIn(List(Range(c, c)))

private enum CastOp(val symbol: Char):
  case Stringify extends CastOp('s')
