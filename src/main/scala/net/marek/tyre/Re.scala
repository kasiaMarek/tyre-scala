package net.marek.tyre

// Re - untyped regular expression, used to buuld expressions from string representations
sealed trait Re

case class ReIn(cs: List[Range]) extends Re:
  override def toString(): String = cs.map:
    case Range(x, y) if x == y => x.toString()
    case Range(x, y) => s"$x-$y"
  .mkString("[","","]")

case class ReNotIn(cs: List[Range]) extends Re:
  override def toString(): String = cs.map:
    case Range(x, y) if x == y => x.toString()
    case Range(x, y) => s"$x-$y"
  .mkString("[^","","]")

case class ReOr(left: Re, right: Re) extends Re:
  override def toString(): String = s"($left|$right)"

case class ReOrS(left: Re, right: Re) extends Re:
  override def toString(): String = s"($left||$right)"

case class ReAnd(left: Re, right: Re) extends Re:
  override def toString(): String = s"($left$right)"

case class ReStar(re: Re) extends Re:
  override def toString(): String = s"$re*"

case class ReOpt(re: Re) extends Re:
  override def toString(): String = s"$re?"

case object ReEpsilon extends Re:
  override def toString(): String = "_"

case class ReHole(index: Int) extends Re:
  override def toString(): String = s"@$index"

object Re:
  def char(c: Char): ReIn = ReIn(List(Range(c, c)))
