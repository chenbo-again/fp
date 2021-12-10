package datastructures.List

sealed trait List[+A]
// 样例对象只有一个实例
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}
	
	def prod(ints: List[Int]): Int = ints match {
		case Nil => 1
		case Cons(x,xs) => x * prod(xs)
	}
	// _*类型注解 将as.tail(Seq) 转化为参数序列
	def apply[A] (as: A*): List[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}
	
}