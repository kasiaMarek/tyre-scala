package net.marek.tyre

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position

object TyreParser extends Parsers:
  type Elem = Token

  private enum Reserved(val char: Char):
    def parser: Parser[Char] = accept(this.toString, { case `char` => char })
    case star extends Reserved('*')
    case plus extends Reserved('+')
    case or extends Reserved('|')
    case lParen extends Reserved('(')
    case rParen extends Reserved(')')
    case lBracket extends Reserved('[')
    case rBracket extends Reserved(']')
    case dash extends Reserved('-')
    case questionMark extends Reserved('?')
    case escape extends Reserved('\\')
    case caret extends Reserved('^')
    case quote extends Reserved('"')
  private object Reserved:
    private val chars = values.map(_.char).toSet
    def contanins(c: Char): Boolean = chars(c)
  private given Conversion[Reserved, Parser[Char]] = _.parser

  import Reserved._

  private val orS = or ~ or
  private val hole = accept("hole", { case Hole(idx) => idx })
  private val literal: Parser[Char] = accept("literal", { case el: Char if !Reserved.contanins(el) => el }) |
    escape ~> accept("escaped literal", { case el: Char if Reserved.contanins(el) => el })
  private val any =
    hole ^^ ReHole.apply
      | lBracket ~> opt(caret) ~ rep1(literal ~ opt(dash ~> literal)) <~ rBracket ^^ {
        case Some(_) ~ list =>
          ReNotIn(list.map {
            case li1 ~ None => Range(li1, li1)
            case start ~ Some(end) => Range(start, end)
          })
        case None ~ list =>
          ReIn(list.map {
            case li1 ~ None => Range(li1, li1)
            case start ~ Some(end) => Range(start, end)
          })
      }
      | literal ^^ { e => Re.char(e) }

  private val consumingExpr: Parser[Re] =
    lParen ~> expr2 <~ rParen | any

  private val repetition: Parser[Rep] =
    star ^^ { _ => Rep.Star } | plus ^^ { _ => Rep.Plus } | questionMark ^^ { _ => Rep.QuestionMark }

  private val expr0: Parser[Re] =
    consumingExpr ~ opt(repetition) ^^ {
      case r ~ Some(Rep.Star) => ReStar(r)
      case r ~ Some(Rep.Plus) => RePlus(r)
      case r ~ Some(Rep.QuestionMark) => ReOpt(r)
      case r ~ None => r
    }

  private val expr1: Parser[Re] =
    expr0 ~ opt(expr1) ^^ {
      case l ~ Some(r) => ReAnd(l, r)
      case l ~ None => l
    }

  private val strictOrMergingOr: Parser[Boolean] =
    orS ^^ { _ => true } | or ^^ { _ => false }

  private val expr2: Parser[Re] =
    expr1 ~ opt(strictOrMergingOr ~ expr2) ^^ {
      case l ~ Some(true ~ r) => ReOrS(l, r)
      case l ~ Some(false ~ r) => ReOr(l, r)
      case l ~ None => l
    }

  private val full: Parser[Re] = expr2 <~ End

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
