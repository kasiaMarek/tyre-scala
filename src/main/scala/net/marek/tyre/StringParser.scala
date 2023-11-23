package net.marek.tyre

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.Conversion

object TyreParser extends Parsers:
  type Elem = Token

  private enum Reserved(val char: Char, val escapedInBrackets: Boolean):
    def parser: Parser[Char] = accept(this.toString, { case `char` => char })
    case star extends Reserved('*', escapedInBrackets = false)
    case plus extends Reserved('+', escapedInBrackets = false)
    case or extends Reserved('|', escapedInBrackets = false)
    case lParen extends Reserved('(', escapedInBrackets = false)
    case rParen extends Reserved(')', escapedInBrackets = false)
    case lBracket extends Reserved('[', escapedInBrackets = false)
    case rBracket extends Reserved(']', escapedInBrackets = true)
    case dash extends Reserved('-', escapedInBrackets = true)
    case questionMark extends Reserved('?', escapedInBrackets = false)
    case escape extends Reserved('\\', escapedInBrackets = true)
    case caret extends Reserved('^', escapedInBrackets = true)
    case quote extends Reserved('"', escapedInBrackets = true)
    case dot extends Reserved('.', escapedInBrackets = false)
    case exclamation extends Reserved('!', escapedInBrackets = false)
  private object Reserved:
    val chars = values.map(_.char).toSet
    def isEscapedInBrackets(c: Char) =
      values.find(_.char == c).exists(_.escapedInBrackets)
  private given Conversion[Reserved, Parser[Char]] = _.parser

  private enum CharClass(val input: Char, val output: List[Range]):
    case space extends CharClass('s', List(' ', '\t', '\n', '\r', '\f', '\u000B'))
    case hSpace
      extends CharClass(
        'h',
        List(' ', '\t', '\u00A0', '\u1680', '\u180E', Range('\u2000', '\u200A'), '\u202F', '\u205F', '\u3000')
      )
    case vSpace extends CharClass('v', List('\n', '\r', '\f', '\u000B', '\u0085', '\u2028', '\u2029'))
    case word extends CharClass('w', List('_', Range('a', 'z'), Range('A', 'Z'), Range('0', '9')))
    case digit extends CharClass('d', List(Range('0', '9')))
    case tab extends CharClass('t', List('\t'))
    case nl extends CharClass('\n', List('\n'))
    case cr extends CharClass('\r', List('\r'))
    case ff extends CharClass('\f', List('\u000C'))
  private object CharClass:
    val vals = values.map(p => p.input -> p.output).toMap
    val negs = values.map(p => p.input.toUpper -> p.output).toMap
    def hasVal(c: Char): Boolean = vals.keySet(c)
    def hasNeg(c: Char): Boolean = negs.keySet(c)

  import NumberHelper.*

  private val unicodeSymbol: Parser[Char] = accept("unicode symbol", { case 'u' => 'u' })
  private val unicodeValue: Parser[Char] = accept("unicode value", { case h: Char if isHex(h) => h })

  private given Conversion[CastOp, Parser[CastOp]] = c => accept(c.toString, { case c.symbol => c })

  import Reserved.*

  private val orS = or ~ or
  private val hole = accept("hole", { case Hole(idx) => idx })
  private val literal: Parser[Char] = accept("literal", { case el: Char if !Reserved.chars(el) => el }) |
    escape ~> accept("escaped literal", { case el: Char if Reserved.chars(el) => el }) |
    escape ~> unicodeSymbol ~> repN(4, unicodeValue) ^^ { case seq => hex(seq: _*).toChar }
  private val charClassIn =
    escape ~> accept("predef class", { case el: Char if CharClass.hasVal(el) => CharClass.vals(el) })
  private val charClassNotIn =
    escape ~> accept("predef class", { case el: Char if CharClass.hasNeg(el) => CharClass.negs(el) })
  private val inBracketSpecial =
    accept("not escaped special", { case el: Char if !Reserved.isEscapedInBrackets(el) => el })

  private val rangeOrLiteralInBrackets =
    literal ~ opt(dash ~> literal) ^^ {
      case li1 ~ None => List(Range(li1, li1))
      case start ~ Some(end) => List(Range(start, end))
    } | inBracketSpecial ^^ { case li => List(Range(li, li)) }
      | charClassIn | charClassNotIn

  private val any =
    hole ^^ ReHole.apply
      | lBracket ~> opt(caret) ~ rep1(rangeOrLiteralInBrackets) <~ rBracket ^^ {
        case Some(_) ~ list => ReNotIn(list.flatten)
        case None ~ list => ReIn(list.flatten)
      }
      | literal ^^ { e => Re.char(e) }
      | charClassIn ^^ { e => ReIn(e) }
      | charClassNotIn ^^ { e => ReNotIn(e) }
      | dot ^^ { _ => ReAny }

  private val consumingExpr: Parser[Re] =
    lParen ~> expr3 <~ rParen | any

  private val repetition: Parser[Rep] =
    star ^^ { _ => Rep.Star } | plus ^^ { _ => Rep.Plus } | questionMark ^^ { _ => Rep.QuestionMark }

  private val conversion: Parser[CastOp] =
    exclamation ~> CastOp.Stringify

  private val expr0: Parser[Re] =
    consumingExpr ~ opt(repetition) ^^ {
      case r ~ Some(Rep.Star) => ReStar(r)
      case r ~ Some(Rep.Plus) => RePlus(r)
      case r ~ Some(Rep.QuestionMark) => ReOpt(r)
      case r ~ None => r
    }

  private val expr1: Parser[Re] =
    expr0 ~ opt(conversion) ^^ {
      case r ~ Some(CastOp.Stringify) => ReCast(r, CastOp.Stringify)
      case r ~ None => r
    }

  private val expr2: Parser[Re] =
    expr1 ~ opt(expr2) ^^ {
      case l ~ Some(r) => ReAnd(l, r)
      case l ~ None => l
    }

  private val strictOrMergingOr: Parser[Boolean] =
    orS ^^ { _ => true } | or ^^ { _ => false }

  private val expr3: Parser[Re] =
    expr2 ~ opt(strictOrMergingOr ~ expr3) ^^ {
      case l ~ Some(true ~ r) => ReOrS(l, r)
      case l ~ Some(false ~ r) => ReOr(l, r)
      case l ~ None => l
    }

  private val full: Parser[Re] = expr3 <~ End

  def apply(input: List[Elem]): Option[Re] =
    full(TokenReader(input :+ End)) match
      case Success(result, next) => Some(result)
      case _ => None

class TokenReader(seq: Seq[Token]) extends Reader[Token]:
  override def atEnd: Boolean = seq.isEmpty
  override def first: Token = seq(0)
  // TODO: implement position
  override def pos: Position =
    new Position {
      override def line: Int = 0
      override def column: Int = 0
      override protected def lineContents: String = ""
    }
  override def rest: Reader[Token] = TokenReader(seq.tail)

object End
case class Hole(idx: Int)

type Token = Char | Hole | End.type

enum Rep:
  case Star, Plus, QuestionMark
