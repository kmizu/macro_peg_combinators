package com.github.kmizu.macro_peg_combinators

import scala.collection.mutable.ListBuffer

object Parsers {
  type Input = String
  case class ~[+A, +B](_1: A, _2: B)
  abstract sealed class ParseResult[+T] {
    /**
      * Drop extra information from this ParseResult.
      * If it was ParseSuccess, result is converted to None,
      * else, message is converted to ""
      * Mainly, this method is used for testing
      *
      * @return if `this.isInstanceOf[ParseSuccess[T]]` `ParseSuccess(None, next)` else `ParseFailure("", next)`
      */
    def drop: ParseResult[Any] = this match {
      case ParseSuccess(_, next) => ParseSuccess(None, next)
      case ParseFailure(_, next) => ParseFailure("", next)
    }
  }
  final case class ParseSuccess[+T](result: T, next: Input) extends ParseResult[T]
  abstract sealed class AbstractFailure extends ParseResult[Nothing] {
    val message: String
  }
  final case class ParseFailure(val message: String, next: Input) extends AbstractFailure

  abstract sealed class Parser[+T] {self =>
    def apply(input: Input): ParseResult[T]
    def |[U >: T](b: Parser[U]): Parser[U] = Alternation(this, b)
    def /[U >: T](b: Parser[U]): Parser[U] = this | b
    def ~[U](b: Parser[U]): Parser[T ~ U] = Sequence(this, b)
    def ? : Parser[Option[T]] = OptionParser(this)
    def * : Parser[List[T]] = RepeatParser(this)
    def + : Parser[List[T]] = Repeat1Parser(this)
    def unary_! : Parser[Any] = NotParser(this)
    def and: Parser[Any] = AndParser(this)
    def evalCC[U](cc: Parser[T] => Parser[U]): Parser[U] = EvalCC(this, cc)
    def display: Parser[T] = new Parser[T] {
      override def apply(input: Input): ParseResult[T] = {
        println("input: " + input)
        self(input)
      }
    }
    def map[U](function: T => U): Parser[U] = MappingParser(this, function)
  }
  def chainl[A](p: Parser[A])(q: Parser[(A, A) => A]): Parser[A] = {
    (p ~ (q ~ p).*).map { case x ~ xs =>
      xs.foldLeft(x) { case (a, f ~ b) =>
          f(a, b)
      }
    }
  }
  type P[+T] = Parser[T]
  def any: AnyParser.type = AnyParser
  def string(literal: String): StringParser = StringParser(literal)
  implicit class RichString(val self: String) extends AnyVal {
    def s: StringParser = StringParser(self)
  }
  def range(ranges: Seq[Char]*): RangedParser = RangedParser(ranges:_*)
  implicit def characterRangesToParser(ranges: Seq[Seq[Char]]): RangedParser = range(ranges:_*)
  def refer[T](parser: => Parser[T]): ReferenceParser[T] = ReferenceParser(() => parser)
  def rewritable[T](parser: Parser[T]): RewritableParser[T] = RewritableParser(parser)
  final case class EvalCC[T, U](parser: Parser[T], cc: Parser[T] => Parser[U]) extends Parser[U] {
    override def apply(input: Input): ParseResult[U] = {
      parser(input) match {
        case ParseSuccess(value, next) =>
          cc(new StringWithValueParser(input.substring(0, input.length - next.length), value))(next)
        case failure@ParseFailure(_, _) =>
          failure
      }
    }
  }
  final case class StringParser(literal: String) extends Parser[String] {
    override def apply(input: Input): ParseResult[String] = {
      if(input.startsWith(literal)) ParseSuccess(literal, input.substring(literal.length))
      else ParseFailure(s"expected $literal", input)
    }
  }
  final case class StringWithValueParser[T](literal: String, value: T) extends Parser[T] {
    override def apply(input: Input): ParseResult[T]  = {
      if(input.startsWith(literal)) ParseSuccess(value, input.substring(literal.length)) else ParseFailure(s"expected $literal", input)
    }
  }
  final case class RangedParser(ranges: Seq[Char]*) extends Parser[String] {
    override def apply(input: Input): ParseResult[String] = {
      if(input.length == 0)
        ParseFailure(s"find EOF, but expeted following characters: $ranges", input)
      else if(ranges.exists(_.find{_ == input.charAt(0)}.isDefined))
        ParseSuccess(input.substring(0, 1), input.substring(1))
      else
        ParseFailure(s"find ${input.substring(0, 1)}, but expected following characters: $ranges", input)
    }
  }
  final case class OptionParser[T](parser: Parser[T]) extends Parser[Option[T]] {
    override def apply(input: Input): ParseResult[Option[T]] = {
      parser(input) match {
        case ParseSuccess(result, next) => ParseSuccess(Some(result), next)
        case ParseFailure(message, next) => ParseSuccess(None, next)
      }
    }
  }
  final case class MappingParser[T, U](parser: Parser[T], function: T => U) extends Parser[U] {
    override def apply(input: Input): ParseResult[U] = {
      parser(input) match {
        case ParseSuccess(result, next) =>
          ParseSuccess(function(result), next)
        case ParseFailure(message, next) =>
          ParseFailure(message, next)
      }
    }
  }
  final case class RepeatParser[T](parser: Parser[T]) extends Parser[List[T]] {
    override def apply(input: Input): ParseResult[List[T]] = {
      var rest = input
      val total = ListBuffer[T]()
      while(true) {
        parser(rest) match {
          case ParseSuccess(result, next) =>
            total += result
            rest = next
          case ParseFailure(message, next) =>
            return ParseSuccess(total.toList, rest)
        }
      }
      throw new RuntimeException("unreachable code")
    }
  }
  final case class Repeat1Parser[T](parser: Parser[T]) extends Parser[List[T]] {
    override def apply(input: Input): ParseResult[List[T]] = {
      var rest = input
      val total = ListBuffer[T]()
      parser(rest) match {
        case ParseSuccess(result, next) =>
          total += result
          rest = next
          while(true) {
            parser(rest) match {
              case ParseSuccess(result, next) =>
                total += result
                rest = next
              case ParseFailure(message, next) =>
                return ParseSuccess(total.toList, rest)
            }
          }
          throw new RuntimeException("unreachable code")
        case ParseFailure(message, next) =>
          return ParseFailure(message, next)
      }
      throw new RuntimeException("unreachable code")
    }
  }
  final case class Alternation[T, U >: T](a: Parser[T], b: Parser[U]) extends Parser[U] {
    override def apply(input: Input): ParseResult[U] = a(input) match {
      case ParseSuccess(result, next) => ParseSuccess(result, next)
      case ParseFailure(message, next) => b(input)
    }
  }
  final case class Sequence[T, U](a: Parser[T], b: Parser[U]) extends Parser[T ~ U] {
    override def apply(input: Input): ParseResult[T ~ U] = a(input) match {
      case ParseSuccess(result1, next1) =>
        b(next1) match {
          case ParseSuccess(result2, next2) =>
            ParseSuccess(new ~(result1, result2), next2)
          case ParseFailure(message, next2) =>
            ParseFailure(message, next2)
        }
      case ParseFailure(message, next) =>
        ParseFailure(message, next)
    }
  }
  final case class ReferenceParser[T](delayedParser: () => Parser[T]) extends Parser[T] {
    override def apply(input: Input): ParseResult[T] = reference(input)
    lazy val reference: Parser[T] = delayedParser()
  }
  final case object AnyParser extends Parser[String] {
    override def apply(input: Input): ParseResult[String] = {
      if(input.length >= 1) ParseSuccess(input.substring(0, 1), input.substring(1)) else ParseFailure("EOF", input)
    }
  }
  final case class AndParser[T](p: Parser[T]) extends Parser[Any] {
    override def apply(input: Input): ParseResult[Any] = p(input) match {
      case ParseSuccess(result, next) => ParseSuccess((), input)
      case ParseFailure(message, next) => ParseFailure(message, input)
    }
  }
  final case class NotParser[T](p: Parser[T]) extends Parser[Any] {
    override def apply(input: Input): ParseResult[Any] = p(input) match {
      case ParseSuccess(result, next) => ParseFailure("", input)
      case ParseFailure(message, next) => ParseSuccess("", input)
    }
  }
  final case class RewritableParser[T](var p: Parser[T]) extends Parser[T] {
    override def apply(input: Input): ParseResult[T] = p(input)
  }
}
