package net.marek.tyre

// Tyre
sealed trait Tyre[R]:
  def rep: Star[R] = Star(this)
  def <*>[S](re: Tyre[S]): Tyre[(R, S)] = And(this, re)
  def <|>[S](re: Tyre[S]): Tyre[Either[R, S]] = Or(this, re)
  def map[S](f: R => S): Tyre[S] = Conv(this, f)

case class Pred(f: Char => Boolean) extends Tyre[Char]

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

object Pred:
  def in(cs: List[Range]): Tyre[Char] = Pred(c => cs.exists(r => r.from <= c && r.to >= c))
  def notIn(cs: List[Range]): Tyre[Char] = Pred(c => cs.forall(r => r.from > c || r.to < c))
  def char(c: Char): Tyre[Char] = Pred(_ == c)
  given Conversion[Char, Tyre[Char]] = char(_)

object Opt:
  def apply[R](re: Tyre[R]): Tyre[Option[R]] =
    val conv: Either[R, Unit] => Option[R] =
      case Left(v) => Some(v)
      case _ => None
    Conv(Or(re, Epsilon), conv)

object OrM:
  def apply[R](left: Tyre[R], right: Tyre[R]): Tyre[R] =
    Conv(Or(left, right), _.merge)

object AndF:
  def apply[R, RT <: Tuple](left: Tyre[R], right: Tyre[RT]): Tyre[R *: RT] =
    Conv(And(left, right), (l, r) => l *: r)
