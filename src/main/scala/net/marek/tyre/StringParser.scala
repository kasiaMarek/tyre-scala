import scala.language.implicitConversions
import scala.quoted.{Expr, Quotes}
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.compiletime.error

extension (sc: StringContext)
  inline def tyre(args: Any*) = ${ tyreImpl('{ sc }, '{ args }) }

private def tyreImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes) =
	val fullTyre = '{ ${ sc }.s(${args}*) }
	'{TyreParser($fullTyre).map(_.tyre)}
	//'{ if "x" == "x" then Some(TyreWrapper(Tyre.char('x'))) else None }

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

	def applyTokenizer(input: String): Option[TyreWrapper] =
		val tokens = tokenize(input)
		expr.apply(tokens) match
			case Success(result, next) if next.atEnd => Some(result)
			case _ => None

	inline def apply(input: String): Option[TyreWrapper] = parse(input)

	inline def parse(input: String): Option[TyreWrapper] =
		def parseRec(input: List[Char], acc: Option[TyreWrapper]): (List[Char], Option[TyreWrapper]) = input match
			case '|' :: tail =>
				val (rest, next) = parseRec(tail, None)
				val tyre = for
					l <- acc
					r <- next
				yield OrTyreWrapper(l, r)
				(rest, tyre)
			case '*' :: tail =>
				val tyre = acc.map(StarTyreWrapper(_))
				(tail, tyre)
			case '(' :: tail =>
				val (rest, in) = parseRec(tail, None)
				val tyre = for
					l <- acc
					r <- in
				yield AndTyreWrapper(l, r)
				parseRec(rest, tyre.orElse(in))
			case ')' :: tail =>
				(tail, acc)
			case c :: tail =>
				val pred = TyreWrapper(Tyre.char(c))
				val tyre = acc.map(AndTyreWrapper(_, pred)).orElse(Some(pred))
				parseRec(tail, tyre)
			case Nil => (Nil, acc)
		if input.isEmpty then Some(TyreWrapper(Epsilon)) else parseRec(input.toList, None)(1)

enum Token:
	case Star
	case Or
	case LParen
	case RParen
	case Literal(char: Char)

trait TyreWrapper:
	type T
	val tyre: Tyre[T]
	override def toString: String = tyre.toString

object TyreWrapper:
	def apply[A](t: Tyre[A]): TyreWrapper = new TyreWrapper:
		type T = A
		val tyre = t

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
