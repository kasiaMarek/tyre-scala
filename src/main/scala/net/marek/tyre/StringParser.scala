import scala.language.implicitConversions
import scala.quoted.{Expr, Quotes, quotes}
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.compiletime.error
// import TyreToExpr.given
import quoted.ToExpr.given

extension (sc: StringContext)
  inline def tyre(args: Any*) = ${ tyreImpl('{ sc }, '{ args }) }

private def tyreImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes) =

	import quotes.reflect.*

	// val fullText = '{${ sc }.s(${args}*)}.valueOrAbort
	
	def createType(tw: TyreWrapper): TypeRepr = tw match
		case _: EpsilonTyreWrapper => TypeRepr.of[Unit]
		case _: PredTyreWrapper => TypeRepr.of[Char]
		case AndTyreWrapper(tw1, tw2) => TypeRepr.of[Tuple2].appliedTo(List(createType(tw1), createType(tw2)))
		case OrTyreWrapper(tw1, tw2) => TypeRepr.of[Either].appliedTo(List(createType(tw1), createType(tw2)))
		case StarTyreWrapper(tw) => TypeRepr.of[List].appliedTo(createType(tw))

	val fullTyre = '{ ${ sc }.s(${args}*) }
	'{TyreParser($fullTyre).map(_.tyre)}

	// def createType2(re: Re): TypeRepr = ???

	// val tw = TyreParser(fullText).get

	// createType2(tw).asType match
	// 	case '[t] => 
	// 		def createTyre(re: Re): Expr[Tyre[t]] = ???
	

	// type TyreType[T <: Re] = T match
	// 	case ReEpsilon => ()
	// ???
	//'{ if "x" == "x" then Some(TyreWrapper(Tyre.char('x'))) else None }

	//def buildExpr[T](t: Tyre[T]): Expr[Tyre[T]] =
		// t match
		// 	case Epsilon => '{ Epsilon }
		// 	case Pred(f) => '{ Pred(???) }
		// 	case Star(re) => '{ Star(${buildExpr(re)}) }
		// 	case And(re1, re2) => '{ And(${buildExpr(re1)}, ${buildExpr(re2)}) }
		// 	case Or(re1, re2) => '{ Or(${buildExpr(re1)}, ${buildExpr(re2)}) }
		
	// createType(tw).asType match
	// 	case '[t] => 
	// 		'{${Expr(tw.tyre)}.asInstanceOf[Tyre[t]]}
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
					case Token.Literal(c) => new PredTyreWrapper(_ == c)
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
		def parseRec(input: List[Char], acc: Option[TyreWrapper], level: Int): ParseState = input match
			case '|' :: tail =>
				val nextS = parseRec(tail, None, level)
				val tyre = for
					l <- acc
					r <- nextS.tyre
				yield OrTyreWrapper(l, r)
				ParseState(nextS.input, tyre, level)
			case '*' :: tail =>
				val tyre = acc.map(StarTyreWrapper(_))
				ParseState(tail, tyre, level)
			case '(' :: tail =>
				val inS = parseRec(tail, None, level+1)
				val tyre = for
					l <- acc
					r <- inS.tyre
				yield AndTyreWrapper(l, r)
				parseRec(inS.input, tyre.orElse(inS.tyre), level)
			case ')' :: tail =>
				ParseState(tail, acc, level-1)
			case c :: tail =>
				val pred = PredTyreWrapper(c == _)
				val tyre = acc.map(AndTyreWrapper(_, pred)).orElse(Some(pred))
				parseRec(tail, tyre, level)
			case Nil => ParseState(Nil, acc, level)
		if input.isEmpty
		then Some(EpsilonTyreWrapper())
		else
			val state = parseRec(input.toList, None, 0)
			if state.level == 0 then state.tyre else None

	case class ParseState(input: List[Char], tyre: Option[TyreWrapper], level: Int)

	// inline def parse2(input: String): Option[Re] =
	// 	def parseRec(input: List[Char], acc: Option[Re], level: Int): ParseState2 = input match
	// 		case '|' :: tail =>
	// 			val nextS = parseRec(tail, None, level)
	// 			val tyre = for
	// 				l <- acc
	// 				r <- nextS.tyre
	// 			yield ReOr(l, r)
	// 			ParseState2(nextS.input, tyre, level)
	// 		case '*' :: tail =>
	// 			val tyre = acc.map(ReStar(_))
	// 			ParseState2(tail, tyre, level)
	// 		case '(' :: tail =>
	// 			val inS = parseRec(tail, None, level+1)
	// 			val tyre = for
	// 				l <- acc
	// 				r <- inS.tyre
	// 			yield ReAnd(l, r)
	// 			parseRec(inS.input, tyre.orElse(inS.tyre), level)
	// 		case ')' :: tail =>
	// 			ParseState2(tail, acc, level-1)
	// 		case c :: tail =>
	// 			val pred = ReChar(c)
	// 			val tyre = acc.map(ReAnd(_, pred)).orElse(Some(pred))
	// 			parseRec(tail, tyre, level)
	// 		case Nil => ParseState2(Nil, acc, level)
	// 	if input.isEmpty
	// 	then Some(ReEpsilon)
	// 	else
	// 		val state = parseRec(input.toList, None, 0)
	// 		if state.level == 0 then state.tyre else None

	// case class ParseState2(input: List[Char], tyre: Option[Re], level: Int)

enum Token:
	case Star
	case Or
	case LParen
	case RParen
	case Literal(char: Char)

sealed trait TyreWrapper:
	type T
	val tyre: Tyre[T]
	override def toString: String = tyre.toString

class EpsilonTyreWrapper extends TyreWrapper:
	type T = Unit
	val tyre = Epsilon

class PredTyreWrapper(pred: Char => Boolean) extends TyreWrapper:
	type T = Char
	val tyre = Pred(pred)

case class AndTyreWrapper(val w1: TyreWrapper, val w2: TyreWrapper) extends TyreWrapper:
	type T = (w1.T, w2.T)
	val tyre = And(w1.tyre, w2.tyre)

case class OrTyreWrapper(val w1: TyreWrapper, val w2: TyreWrapper) extends TyreWrapper:
	type T = Either[w1.T, w2.T]
	val tyre = Or(w1.tyre, w2.tyre)

case class StarTyreWrapper(val w: TyreWrapper) extends TyreWrapper:
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




