import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{Position, Reader}
import scala.sys.process.Parser
import scala.compiletime.ops.any

object TyreParser extends RegexParsers:
	private val star = literal("*")
	private val or = literal("|")
	private val lParen = literal("(")
	private val rParen = literal(")")
	private val any = regex("[^*|()]".r)
	private val end = regex("$".r)
	private val pred: Parser[Re] =
		any ^^ {str => ReOneOf(List(str.head))}

	private val consumingExpr: Parser[Re] =
		lParen ~> expr <~ rParen | pred

	def leftOfExpr: Parser[Re => Re] =
		or ~> expr ^^ {case r2 => ReOr(_, r2)} |
		star ^^ {case _ => ReStar(_)} |
		expr ^^ {case r2 => ReAnd(_, r2)}

	def expr: Parser[Re] =
		consumingExpr ~ opt(leftOfExpr) ^^ { case r ~ Some(f) => f(r)
																				 case r ~ None => r }

	def apply(input: String): Option[Re] =
		parseAll(expr, input) match
			case Success(result, next) => Some(result)
			case _ => None
