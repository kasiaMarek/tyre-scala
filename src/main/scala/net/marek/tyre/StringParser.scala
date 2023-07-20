import scala.quoted.{Expr, Quotes}
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.compiletime.error

extension (sc: StringContext)
  inline def tyre(args: Any*) = ${ tyreImpl('{ sc }, '{ args }) }

private def tyreImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes) =
	val fullTyre = '{ ${ sc }.s(${args}) }
	'{ TyreParser($fullTyre).map(_.tyre).getOrElse(error("Not a valid tyre pattern.")) }

object TyreParser extends Parsers:
	type Elem = Token
	def tokenize(input: String): Reader[Token] =
		val seq =
			input.toCharArray().map:
				case '*' => Token.Star
				case '|' => Token.Or
				case '(' => Token.RParen
				case ')' => Token.LParen
				case c => Token.Literal(c)
			.toSeq
		TokenReader(seq)
	def pred: Parser[TyreWrapper] =
		elem("pred", _.isInstanceOf[Token.Literal])
			^^ {
					case Token.Literal(c) =>
						new TyreWrapper:
							type T = Char
							val tyre = Pred(c == _)
					case _ => ???
				}
	def expr: Parser[TyreWrapper] =
		pred |
		(expr ~ expr) ^^ { case w1 ~ w2 => AndTyreWrapper(w1, w2) } |
		((expr <~ elem(Token.Or)) ~ expr) ^^ { case w1 ~ w2 => OrTyreWrapper(w1, w2) } |
		elem(Token.LParen) ~> expr <~ elem(Token.RParen) |
		expr <~ elem(Token.Star) ^^ { StarTyreWrapper(_) }

	def apply(input: String): Option[TyreWrapper] =
		val tokens = tokenize(input)
		expr.apply(tokens) match
			case Success(result, next) if next.atEnd => Some(result)
			case _ => None

enum Token:
	case Star
	case Or
	case LParen
	case RParen
	case Literal(char: Char)

trait TyreWrapper:
	type T
	val tyre: Tyre[T]

class AndTyreWrapper(val w1: TyreWrapper, val w2: TyreWrapper) extends TyreWrapper:
	type T = (w1.T, w2.T)
	val tyre = And(w1.tyre, w2.tyre)

class OrTyreWrapper(val w1: TyreWrapper, val w2: TyreWrapper) extends TyreWrapper:
	type T = Either[w1.T, w2.T]
	val tyre = Or(w1.tyre, w2.tyre)

class StarTyreWrapper(val w: TyreWrapper) extends TyreWrapper:
	type T = List[w.T]
	val tyre = Star(w.tyre)

class TokenReader(seq: Seq[Token]) extends Reader[Token]:
	override def atEnd: Boolean = seq.isEmpty
	override def first: Token = seq(0)
	override def pos: Position =
		new Position {
			override def line: Int = 0
			override def column: Int = 0
			override protected def lineContents: String = ""
		}
	override def rest: Reader[Token] = TokenReader(seq.tail)
