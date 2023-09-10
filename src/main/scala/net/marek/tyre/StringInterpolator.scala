package net.marek.tyre

import scala.language.implicitConversions
import scala.quoted.{quotes, Expr, Quotes, Type, Varargs}

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
    case ReOneOf(cs) => '{ OneOf(${ Expr(cs) }) }
    case ReAnd(re1, re2) =>
      toTyre(re1) match
        case '{ $ree1: Tyre[t1] } =>
          toTyre(re2) match
            case '{ $ree2: Tyre[t2] } => '{ And(${ ree1 }, ${ ree2 }) }
    case ReOr(re1, re2) =>
      toTyre(re1) match
        case '{ $ree1: Tyre[t1] } =>
          toTyre(re2) match
            case '{ $ree2: Tyre[t2] } => '{ Or(${ ree1 }, ${ ree2 }) }
    case ReStar(re) =>
      toTyre(re) match
        case '{ $ree: Tyre[t1] } => '{ Star(${ ree }) }
    case ReHole(i) => tyreArgs(i)

  toTyre(re)
