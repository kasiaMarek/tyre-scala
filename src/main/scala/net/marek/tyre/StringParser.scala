import scala.language.implicitConversions
import scala.quoted.{Expr, Quotes, quotes}
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.compiletime.error
import quoted.ToExpr.given
import scala.quoted.Type
import ReToExpr.given
import scala.quoted.Varargs

extension (inline sc: StringContext)
  inline def tyre(inline args: Any*) = ${ tyreImpl('{ sc }, '{ args }) }

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

	val fullTyre = parts.zipAll(args_, "", "").mkString

	val re = TyreParser(fullTyre).getOrElse(throw new RuntimeException("incorrect tyre expression"))
	
	createType(re).asType match
	 	case '[t] => '{${Expr(re)}.asInstanceOf[Tyre[t]]}


object TyreParser:
	def apply(input: String): Option[Re] = parse(input)
	inline def parse(input: String): Option[Re] =
		def parseRec(input: List[Char], acc: Option[Re], level: Int): ParseState = input match
			case '|' :: tail =>
				val nextS = parseRec(tail, None, level)
				val tyre = for
					l <- acc
					r <- nextS.tyre
				yield ReOr(l, r)
				ParseState(nextS.input, tyre, level)
			case '*' :: tail =>
				val tyre = acc.map(ReStar(_))
				ParseState(tail, tyre, level)
			case '(' :: tail =>
				val inS = parseRec(tail, None, level+1)
				val tyre = for
					l <- acc
					r <- inS.tyre
				yield ReAnd(l, r)
				parseRec(inS.input, tyre.orElse(inS.tyre), level)
			case ')' :: tail =>
				ParseState(tail, acc, level-1)
			case c :: tail =>
				val pred = ReOneOf(List(c))
				val tyre = acc.map(ReAnd(_, pred)).orElse(Some(pred))
				parseRec(tail, tyre, level)
			case Nil => ParseState(Nil, acc, level)
		if input.isEmpty
		then Some(ReEpsilon)
		else
			val state = parseRec(input.toList, None, 0)
			if state.level == 0 then state.tyre else None

	case class ParseState(input: List[Char], tyre: Option[Re], level: Int)

enum Token:
	case Star
	case Or
	case LParen
	case RParen
	case Literal(char: Char)

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




