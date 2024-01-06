package net.marek.tyre.pattern

import net.marek.tyre.*

private object Opt:
  def apply[R](re: Tyre[R]): Tyre[Option[R]] =
    val conv: Either[R, Unit] => Option[R] =
      case Left(v) => Some(v)
      case _ => None
    Conv(Or(re, Epsilon), conv)

private object OrM:
  def merge[R1, R2](e: Either[R1, R2]): R1 | R2 = e match
    case Left(v) => v
    case Right(v) => v
  def apply[R1, R2](left: Tyre[R1], right: Tyre[R2]): Tyre[R1 | R2] =
    Conv(Or(left, right), merge)

private object OrMWithSingle:
  def apply[R2](c: Char & Singleton, right: Tyre[R2]): Tyre[c.type | R2] =
    OrM(Single(c), right)

private object AndF:
  def apply[R, RT <: Tuple](left: Tyre[R], right: Tyre[RT]): Tyre[R *: RT] =
    Conv(And(left, right), (l, r) => l *: r)

private object Cast:
  // TODO: restrict input type
  private def string(x: Any): String = x match
    case h *: t => s"${string(h)}${string(t)}"
    case h :: t => s"${string(h)}${string(t)}"
    case Some(x) => s"${string(x)}"
    case Left(x) => s"${string(x)}"
    case Right(x) => s"${string(x)}"
    case EmptyTuple => ""
    case Nil => ""
    case None => ""
    case s => s.toString
  def apply[R](re: Tyre[R], op: CastOp) = op match
    case CastOp.Stringify => re.map(string)
