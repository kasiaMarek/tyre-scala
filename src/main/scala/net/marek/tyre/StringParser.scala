package net.marek.tyre

import scala.util.parsing.combinator.RegexParsers

object TyreParser extends RegexParsers:
  private val star = literal("*")
  private val or = literal("|")
  private val lParen = literal("(")
  private val rParen = literal(")")
  private val lBracket = literal("[")
  private val rBracket = literal("]")
  private val dash = literal("-")
  private val questionMark = literal("?")
  private val escape = literal("""\""")
  private val literal = regex("""[^*|()\\\]\[]""".r) ^^ { _.head }
  private val any =
    escape ~> (star | or | lParen | rParen | escape | lBracket | rBracket) ^^ { s => ReOneOf(List(s.head)) }
      | lBracket ~> rep1(literal ~ opt(dash ~> literal)) <~ rBracket ^^ { list =>
        ReOneOf(list.flatMap {
          case li1 ~ None => List(li1)
          case start ~ Some(end) =>
            (start to end).toList
        })
      }
      | literal ^^ { e => ReOneOf(List(e)) }

  private val consumingExpr: Parser[Re] =
    lParen ~> expr2 <~ rParen | any

  private val starOrQuestionMark: Parser[Boolean] =
      star ^^ {_ => true} | questionMark ^^ {_ => false}

  private val expr0: Parser[Re] =
    consumingExpr ~ opt(starOrQuestionMark) ^^ {
      case r ~ Some(true) => ReStar(r)
      case r ~ Some(false) => ReOr(r, ReEpsilon)
      case r ~ None => r
    }

  private val expr1: Parser[Re] =
    expr0 ~ opt(expr1) ^^ {
      case l ~ Some(r) => ReAnd(l, r)
      case l ~ None => l
    }

  private val expr2: Parser[Re] =
    expr1 ~ opt(or ~> expr2) ^^ {
      case l ~ Some(r) => ReOr(l, r)
      case l ~ None => l
    }

  def apply(input: String): Option[Re] =
    parseAll(expr2, input) match
      case Success(result, next) => Some(result)
      case _ => None
