import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{Position, Reader}
import scala.sys.process.Parser

object TyreParser extends Parsers:

	type Elem = Token
	def tokenize(input: String): TokenReader =
		val seq =
			input.toCharArray().map:
				case '*' => Token.Star
				case '|' => Token.Or
				case '(' => Token.LParen
				case ')' => Token.RParen
				case c => Token.Literal(c)
			.toSeq :+ Token.End
		TokenReader(seq)
	def pred: Parser[Re] =
		elem("pred", _.isInstanceOf[Token.Literal])
			^^ {
					case Token.Literal(c) => ReOneOf(List(c))
					case _ => ???
				}
	def consumingExpr: Parser[Re] =
		elem(Token.LParen) ~> expr <~ elem(Token.RParen) | pred

	def leftOfExpr: Parser[Re => Re] =
		elem(Token.Or) ~> expr ^^ {case r2 => ReOr(_, r2)} |
		elem(Token.Star) ^^ {case _ => ReStar(_)} |
		expr ^^ {case r2 => ReAnd(_, r2)}

	def expr: Parser[Re] =
		consumingExpr ~ opt(leftOfExpr) ^^ { case r ~ Some(f) => f(r)
																				 case r ~ None => r }

	def fullRe: Parser[Re] = expr <~ elem(Token.End)

	def apply(input: String): Option[Re] =
		val tokens = tokenize(input)
		fullRe.apply(tokens) match
			case Success(result, next) => Some(result)
			case _ => None

enum Token:
	case Star
	case Or
	case LParen
	case RParen
	case Literal(char: Char)
	case End

// TODO: implement token reader w/ correct positions
case class TokenReader(seq: Seq[Token]) extends Reader[Token]:
	override def atEnd: Boolean = seq.isEmpty
	override def first: Token = seq(0)
	override def pos: Position =
		new Position {
			override def line: Int = 0
			override def column: Int = 0
			override protected def lineContents: String = ""
		}
	override def rest: Reader[Token] = TokenReader(seq.tail)
