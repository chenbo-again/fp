package practice

import datastructures.Stream._

//scala> def if2[A] (cond: Boolean, onTrue: => A, onFalse: => A): A =
//     | if (cond)  onTrue else onFalse
//def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A
//
//scala> if2(true,
//     |  println("a"),
//     |  println("b")
//     | )
//a
//
//scala> def if3[A] (cond: Boolean, onTrue: A, onFalse: A): A =
//     | if (cond)  onTrue else onFalse
//def if3[A](cond: Boolean, onTrue: A, onFalse: A): A
//
//scala> if3(true,
//     |  println("a"),
//     |  println("b")
//     | )
//a
//b

/***
 * unsafe{函数的参数会被立即求值}
 * 而函数只在调用时会被求值
 */

object ch5 {
	// 5.1 ~ 5.7 Stream/Stream.scala
	
	// 无限流
	def constant[A] (a: A): Stream[A] =
		Stream.cons(a, constant(a))
	
	def from(n: Int): Stream[Int] =
		Stream.cons(n, from(n+1))
	
	def fibs(a: Int = 0, b: Int = 1): Stream[Int] =
		Stream.cons(a, fibs(b, a+b))
		
	def unfold[A, S] (z: S)(f: S => Option[(A, S)]): Stream[A] = {
		f(z) match {
			case None => Empty
			case Some((a, s)) => Stream.cons(a, unfold(s)(f))
		}
	}
	
	def unfoldOnes: Stream[Int] =
		unfold(0)(_ => Some(1, 0))
		
	def unfoldConstant(n: Int): Stream[Int] =
		unfold(0)(_ => Some(n, 0))
		
	def unfoldFrom(n: Int): Stream[Int] =
		unfold(n)(s => Some(s, s+1))
		
	// 必须得有个 case
	def unfoldFib: Stream[Int] =
		unfold{(0, 1)}{
			case (a, b) => Some(a, (b, a+b))
		}
		
	// 5.13 部分在 Stream.scala中
	
	def zipWith[A,B,C] (sa: Stream[A],sb: Stream[B])(f: (A, B) => C): Stream[C] =
		unfold((sa, sb)) {
			case (Empty, _) => None
			case (_, Empty) => None
			case (Cons(ha, ta), Cons(hb, tb)) =>	Some(f(ha(),hb()), (ta(), tb()))
		}
}
