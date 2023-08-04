// import scala.quoted.*

// object TyreToExpr:
//   given ToExpr[Epsilon.type] with
//     def apply(t: Epsilon.type)(using Quotes) =
//     	t match
//     	 	case Epsilon => '{Epsilon}

//   given ToExpr[Pred] with
//     def apply(t: Pred)(using Quotes) =
//     	t match
//     	 	case Pred(t) => '{Pred(???)}

//   given toExprAnd[T1, T2](using ToExpr[Tyre[T1]], ToExpr[Tyre[T2]]): ToExpr[Tyre[(T1, T2)]] with
//   	def apply(t: Tyre[(T1, T2)])(using Quotes) =
//   		t match
//   			case And(t1, t2) => '{And(${Expr(t1)}, ${Expr(t2)})}

//   given toExprOr[T1, T2](using ToExpr[Tyre[T1]], ToExpr[Tyre[T2]]): ToExpr[Tyre[Either[T1, T2]]] with
//   	def apply(t: Tyre[Either[T1, T2]])(using Quotes) =
//   		t match
//   			case Or(t1, t2) => '{Or(${Expr(t1)}, ${Expr(t2)})}

//   given toExprStar[T1](using ToExpr[Tyre[T1]]): ToExpr[Tyre[List[T1]]] with
//   	def apply(t: Tyre[List[T1]])(using Quotes) =
//   		t match
//   			case Star(t) => '{Star(${Expr(t)})}