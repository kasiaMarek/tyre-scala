// Tyre
sealed trait Tyre[R]:
	def rep = Star(this)
	def <*>[S](re: Tyre[S]) = And(this, re)
	def <|>[S](re: Tyre[S]) = Or(this, re)
	def map[S](f: R => S) = Conv(this, f)
case class OneOf(cs: List[Char]) extends Tyre[Char]:
	override def toString: String = "Pred(_)"
case class Or[R1, R2](right: Tyre[R1], left: Tyre[R2]) extends Tyre[Either[R1, R2]]
case class And[R1, R2](right: Tyre[R1], left: Tyre[R2]) extends Tyre[(R1, R2)]
case class Star[R](re: Tyre[R]) extends Tyre[List[R]]
case object Epsilon extends Tyre[Unit]
case class Conv[R1, R2](tyre: Tyre[R1], f: R1 => R2) extends Tyre[R2]

object Tyre:
	def char(c : Char) = OneOf(List(c))
	given Conversion[Char, Tyre[Char]] = char(_)

//Re
sealed trait Re
case class ReChar(c: Char) extends Re
case class ReOr(right: Re, left: Re) extends Re
case class ReAnd(right: Re, left: Re) extends Re
case class ReStar(re: Re) extends Re
case object ReEpsilon extends Re
