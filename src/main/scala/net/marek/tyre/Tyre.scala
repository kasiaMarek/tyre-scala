package net.marek.tyre

import net.marek.tyre.utils.Range

// Tyre
sealed trait Tyre[+R]:
  def rep: Tyre[List[R]] = Star(this)
  def <*>[S](re: Tyre[S]): Tyre[(R, S)] = And(this, re)
  def <|>[S](re: Tyre[S]): Tyre[Either[R, S]] = Or(this, re)
  def map[S](f: R => S): Tyre[S] = Conv(this, f)

private case class Pred(f: Char => Boolean) extends Tyre[Char]

private case class Or[+R1, +R2](left: Tyre[R1], right: Tyre[R2]) extends Tyre[Either[R1, R2]]:
  override def toString(): String = s"($left|$right)"

private case class And[+R1, +R2](left: Tyre[R1], right: Tyre[R2]) extends Tyre[(R1, R2)]:
  override def toString(): String = s"($left$right)"

private case class Star[+R](tyre: Tyre[R]) extends Tyre[List[R]]:
  override def toString(): String = s"$tyre*"

private case object Epsilon extends Tyre[Unit]:
  override def toString(): String = "_"

private case class Conv[R1, +R2](tyre: Tyre[R1], f: R1 => R2) extends Tyre[R2]:
  override def toString(): String = s"f($tyre)"

object Pred:
  def pred(f: Char => Boolean): Tyre[Char] = Pred(f)
  def any: Tyre[Char] = Pred(_ => true)
  def in(cs: List[Range]): Tyre[Char] = Pred(c => cs.exists(r => r.from <= c && r.to >= c))
  def notIn(cs: List[Range]): Tyre[Char] = Pred(c => cs.forall(r => r.from > c || r.to < c))
  def char(c: Char): Tyre[Char] = Pred(_ == c)
  given Conversion[Char, Tyre[Char]] = char(_)
