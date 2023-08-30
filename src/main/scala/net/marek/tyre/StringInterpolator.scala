import scala.language.implicitConversions
import scala.quoted.{Expr, Quotes, Type, Varargs, quotes}
import scala.compiletime.error

extension (inline sc: StringContext)
  transparent inline def tyre(inline args: Any*) = ${ tyreImpl('{ sc }, '{ args }) }

private def tyreImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes) =

  import quotes.reflect.*
  
  def createType(re: Re): TypeRepr = re match
    case ReEpsilon => TypeRepr.of[Unit]
    case ReOneOf(_) => TypeRepr.of[Char]
    case ReAnd(re1, re2) => TypeRepr.of[Tuple2].appliedTo(List(createType(re1), createType(re2)))
    case ReOr(re1, re2) => TypeRepr.of[Either].appliedTo(List(createType(re1), createType(re2)))
    case ReStar(re) => TypeRepr.of[List].appliedTo(createType(re))

  val parts: Seq[String] = sc match
    case '{ StringContext($t: _*) } =>
      t match
        case Varargs(parts) => parts.map(_.valueOrAbort)

  val args_ =
    args match
      case Varargs(pars) =>
        pars.map: 
          case '{ $str: String } => str.valueOrAbort
          case _ => throw new RuntimeException("only string values can be interpolated")

  val fullTyre = parts.zipAll(args_, "", "").map{ case (a1, a2) => a1 ++ a2 }.mkString

  val re = TyreParser(fullTyre).getOrElse(throw new RuntimeException("incorrect tyre expression"))

  def toTyre(re: Re)(using Quotes): Expr[Tyre[?]] = re match
    case ReEpsilon => '{ Epsilon }
    case ReOneOf(cs) => '{ OneOf( ${Expr(cs)}) }
    case ReAnd(re1, re2) =>
      toTyre(re1) match
        case '{$ree1 : Tyre[t1]} =>
          toTyre(re2) match
            case '{$ree2 : Tyre[t2]} => '{ And(${ree1}, ${ree2}) }
    case ReOr(re1, re2) =>
      toTyre(re1) match
        case '{$ree1 : Tyre[t1]} =>
          toTyre(re2) match
            case '{$ree2 : Tyre[t2]} => '{ Or(${ree1}, ${ree2}) }
    case ReStar(re) =>
      toTyre(re) match
        case '{$ree : Tyre[t1]} => '{ Star(${ree}) }

  createType(re).asType match
     case '[t] => '{${toTyre(re)}.asInstanceOf[Tyre[t]]}
