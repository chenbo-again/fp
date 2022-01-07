package parser

import scala.language.{higherKinds, implicitConversions}

object Parser {
	
	trait Parsers [ParseError, Parser[+_]] { self =>
		def run[A] (p: Parser[A])(input: String): Either[ParseError, A]
		
		implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
		
//		implicit def asStringParser[A](a: A)(implicit f: A =>
//			Parser[String]): ParserOps[String] = ParserOps(f(a))
		
		implicit def string(s: String): Parser[String]
		
		def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
		
		def or[A] (s1: Parser[A], s2: Parser[A]): Parser[A]
		
		def product[A,B](p1: Parser[A], p2: Parser[B]): Parser[(A,B)]
		
		def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
		
		def many[A] (p: Parser[A]): Parser[List[A]]
		
		def map[A,B](a: Parser[A]) (f: A => B): Parser[B]
		
		def succeed[A](a: A): Parser[A] = string("").map(_ => a)
		
		def slice[A](p: Parser[A]): Parser[String]
		
		def map2[A,B,C](p1: Parser[A], p2: Parser[B]) (f: (A,B) => C): Parser[C] =
			(p1 ** p2).map(p => f(p._1, p._2))
		
		def many1[A](p: Parser[A]): Parser[List[A]] =
			(p ** p.many).map(x => x._1 :: x._2)
			
		// run(a ** b)(s) = (run(a)())
		
		// 糖
		case class ParserOps[A] (p: Parser[A]) {
			def | [B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
			def or [B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
			
			def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
			def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
			
			def map[B](f: A => B): Parser[B] = self.map(p)(f)
			
			def slice: Parser[String] = self.slice(p)
			
			def many: Parser[List[A]] = self.many(p)
		}
	}
	
}
