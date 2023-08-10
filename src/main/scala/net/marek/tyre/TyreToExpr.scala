import scala.quoted.*

object TyreToExpr:

	given [T: Type]: ToExpr[Tyre[T]] with
		def apply(t: Tyre[T])(using Quotes): Expr[Tyre[T]] = t match
			case Epsilon => '{Epsilon}
			case OneOf(cs) => '{OneOf(${Expr(cs)})}
			case And(t1, t2) => '{And(${Expr(t1)}, ${Expr(t2)})}
			case Or(t1, t2) => '{Or(${Expr(t1)}, ${Expr(t2)})}
			case Star(t) => '{Star(${Expr(t)})}

	// given ToExpr[Epsilon.type] with
	// 	def apply(t: Epsilon.type)(using Quotes) =
	// 		t match
	// 			case Epsilon => '{Epsilon}

	// given ToExpr[OneOf] with
	// 	def apply(t: OneOf)(using Quotes) =
	// 		t match
	// 			case OneOf(cs) => '{OneOf(${Expr(cs)})}

	// given toExprAnd[T1: Type, T2: Type](using ToExpr[Tyre[T1]], ToExpr[Tyre[T2]]): ToExpr[And[T1, T2]] with
	// 	def apply(t: And[T1, T2])(using Quotes) =
	// 		t match
	// 			case And(t1, t2) => '{And(${Expr(t1)}, ${Expr(t2)})}

	// given toExprOr[T1: Type, T2: Type](using ToExpr[Tyre[T1]], ToExpr[Tyre[T2]]): ToExpr[Or[T1, T2]] with
	// 	def apply(t: Or[T1, T2])(using Quotes) =
	// 		t match
	// 			case Or(t1, t2) => '{Or(${Expr(t1)}, ${Expr(t2)})}

	// given toExprStar[T: Type](using ToExpr[Tyre[T]]): ToExpr[Star[T]] with
	// 	def apply(t: Star[T])(using Quotes) =
	// 		t match
	// 			case Star(t) => '{Star(${Expr(t)})}

object ReToExpr:

	given ToExpr[Re] with
		def apply(re: Re)(using Quotes): Expr[Re] = re match
			case ReEpsilon => '{ReEpsilon}
			case ReChar(c) => '{ReChar(${Expr(c)})}
			case ReAnd(re1, re2) => '{ReAnd(${Expr(re1)}, ${Expr(re2)})}
			case ReOr(re1, re2) => '{ReOr(${Expr(re1)}, ${Expr(re2)})}
			case ReStar(r) => '{ReStar(${Expr(r)})}
