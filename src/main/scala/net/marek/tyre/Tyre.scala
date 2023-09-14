package net.marek.tyre

// Tyre
sealed trait Tyre[R]:
  def rep: Star[R] = Star(this)
  def <*>[S](re: Tyre[S]): Tyre[(R, S)] = And(this, re)
  def <|>[S](re: Tyre[S]): Tyre[Either[R, S]] = Or(this, re)
  def map[S](f: R => S): Tyre[S] = Conv(this, f)

case class OneOf(cs: List[Char]) extends Tyre[Char]:
  override def toString(): String = cs match
    case c :: Nil => c.toString()
    case list => s"[${list.sorted.mkString}]"

case class Or[R1, R2](left: Tyre[R1], right: Tyre[R2]) extends Tyre[Either[R1, R2]]:
  override def toString(): String = s"($left|$right)"

case class And[R1, R2](left: Tyre[R1], right: Tyre[R2]) extends Tyre[(R1, R2)]:
  override def toString(): String = s"($left$right)"

case class Star[R](tyre: Tyre[R]) extends Tyre[List[R]]:
  override def toString(): String = s"$tyre*"

case object Epsilon extends Tyre[Unit]:
  override def toString(): String = "_"

case class Conv[R1, R2](tyre: Tyre[R1], f: R1 => R2) extends Tyre[R2]:
  override def toString(): String = s"f($tyre)"

object Opt:
  def apply[R](re: Tyre[R]): Tyre[Option[R]] =
    val conv: Either[R, Unit] => Option[R] =
      case Left(v) => Some(v)
      case _ => None
    Conv(Or(re, Epsilon), conv)

object Tyre:
  def char(c: Char): OneOf = OneOf(List(c))
  given Conversion[Char, Tyre[Char]] = char(_)
