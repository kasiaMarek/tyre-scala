package net.marek.tyre.pattern

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Varargs
import scala.quoted.ToExpr
import scala.quoted.quotes
import net.marek.tyre.*
import net.marek.tyre.utils.Range.given

extension (inline sc: StringContext) transparent inline def tyre(inline args: Any*) = ${ tyreImpl('{ sc }, '{ args }) }

private def tyreImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes) =

  import quotes.reflect.*

  val parts: Seq[String] = sc match
    case '{ StringContext($t: _*) } =>
      t match
        case Varargs(parts) => parts.map(_.valueOrAbort)

  val args_ =
    args match
      case Varargs(pars) =>
        pars.zipWithIndex.map:
          case ('{ $str: String }, _) => str.valueOrAbort
          case ('{ $tyre: Tyre[t] }, idx) => Hole(idx)
          case _ => throw new RuntimeException("only string and tyre values can be interpolated")

  val tyreArgs =
    args match
      case Varargs(pars) =>
        pars.zipWithIndex.collect { case ('{ $tyre: Tyre[t] }, idx) =>
          idx -> tyre
        }.toMap

  val fullTyre = parts.zipAll(args_, "", "").foldLeft(List.empty[Token]) { case (acc, (el1, el2)) =>
    val right =
      el2 match
        case s: String => s.toList
        case h: Hole => List(h)
    acc ++ el1.toList ++ right
  }

  val re = TyreParser(fullTyre).getOrElse(throw new RuntimeException("incorrect tyre expression"))

  def toTyre(re: Re)(using Quotes): Expr[Tyre[?]] = re match
    case ReEpsilon => '{ Epsilon }
    case ReAny => '{ Pred.any }
    case ReSingle(s) => '{ Pred.single(${ Expr(s) }) }
    case ReIn(cs) => '{ Pred.in(${ Expr(cs) }) }
    case ReNotIn(cs) => '{ Pred.notIn(${ Expr(cs) }) }
    case ReAnd(re1, re2) =>
      toTyre(re1) match
        case '{ $ree1: Tyre[t1] } =>
          toTyre(re2) match
            case '{ $ree2: Tyre[t2 *: t3] } => '{ AndF(${ ree1 }, ${ ree2 }) }
            case '{ $ree2: Tyre[t2] } => '{ And(${ ree1 }, ${ ree2 }) }
    case ReOr(re1, re2) =>
      toTyre(re1) match
        case '{ $ree1: Tyre[t1] } =>
          toTyre(re2) match
            case '{ $ree2: Tyre[t2] } => '{ OrM(${ ree1 }, ${ ree2 }) }
    case ReOrS(re1, re2) =>
      toTyre(re1) match
        case '{ $ree1: Tyre[t1] } =>
          toTyre(re2) match
            case '{ $ree2: Tyre[t2] } => '{ Or(${ ree1 }, ${ ree2 }) }
    case ReStar(re) =>
      toTyre(re) match
        case '{ $ree: Tyre[t1] } => '{ Star(${ ree }) }
    case RePlus(re) =>
      toTyre(re) match
        case '{ $ree: Tyre[t1] } => '{ And(${ ree }, Star(${ ree })) }
    case ReOpt(re) =>
      toTyre(re) match
        case '{ $ree: Tyre[t1] } => '{ Opt(${ ree }) }
    case ReLiteralConv(ReIn(cs)) =>
      assert(cs.map(_.size).sum <= 16, "Character class to large to create union type")
      val allChars = cs.flatMap(_.getChars)

      def loop(cs: List[Char]): Expr[?] =
        cs match
          case Nil => '{ ??? }
          case c :: Nil => '{ Union.single(${ Expr(c) }) }
          case c :: tail =>
            loop(tail) match
              case '{ $acce: t } =>
                '{ Union[t](${ Expr(c) }) }

      loop(allChars) match
        case '{ $acce: t } =>
          '{ Pred.in(${ Expr(cs) }).cast[t] }

    case ReCast(re, cast) =>
      toTyre(re) match
        case '{ $ree: Tyre[t1] } => '{ Cast(${ ree }, ${ Expr(cast) }) }
    case ReHole(i) => tyreArgs(i)

  toTyre(re) match
    case '{ $re: Tyre[t] } => '{ ${ re }.asInstanceOf[Tyre[t]] }

private given ToExpr[CastOp] with
  def apply(c: CastOp)(using Quotes) = c match
    case CastOp.Stringify => '{ CastOp.Stringify }

private object Union:
  def single(c: Char & Singleton): c.type = c
  def apply[R2](c: Char & Singleton): c.type | R2 = c
