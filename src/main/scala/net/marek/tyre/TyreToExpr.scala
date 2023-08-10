import scala.quoted.*

object ReToExpr:

	given ToExpr[Re] with
		def apply(re: Re)(using Quotes): Expr[Re] = re match
			case ReEpsilon => '{ReEpsilon}
			case ReOneOf(cs) => '{ReOneOf(${Expr(cs)})}
			case ReAnd(re1, re2) => '{ReAnd(${Expr(re1)}, ${Expr(re2)})}
			case ReOr(re1, re2) => '{ReOr(${Expr(re1)}, ${Expr(re2)})}
			case ReStar(r) => '{ReStar(${Expr(r)})}
