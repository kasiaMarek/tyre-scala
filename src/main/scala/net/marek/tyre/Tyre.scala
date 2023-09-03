package net.marek.tyre

// Tyre
sealed trait Tyre[R]:
  def rep = Star(this)
  def <*>[S](re: Tyre[S]) = And(this, re)
  def <|>[S](re: Tyre[S]) = Or(this, re)
  def map[S](f: R => S) = Conv(this, f)
case class OneOf(cs: List[Char]) extends Tyre[Char]:
  override def toString: String = s"OneOf($cs)"
case class Or[R1, R2](right: Tyre[R1], left: Tyre[R2]) extends Tyre[Either[R1, R2]]
case class And[R1, R2](right: Tyre[R1], left: Tyre[R2]) extends Tyre[(R1, R2)]
case class Star[R](re: Tyre[R]) extends Tyre[List[R]]
case object Epsilon extends Tyre[Unit]
case class Conv[R1, R2](tyre: Tyre[R1], f: R1 => R2) extends Tyre[R2]

object Tyre:
  def char(c: Char) = OneOf(List(c))
  given Conversion[Char, Tyre[Char]] = char(_)

//Re
sealed trait Re
case class ReOneOf(cs: List[Char]) extends Re:
  override def toString(): String = cs match
    case c :: Nil => c.toString()
    case list => s"[${list.mkString}]"

case class ReOr(right: Re, left: Re) extends Re:
  override def toString(): String = s"($right|$left)"
case class ReAnd(right: Re, left: Re) extends Re:
  override def toString(): String = s"($right$left)"
case class ReStar(re: Re) extends Re:
  override def toString(): String = s"$re*"
case object ReEpsilon extends Re:
  override def toString(): String = "_"

object Re:
  def char(c: Char) = ReOneOf(List(c))
