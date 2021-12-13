package datastructures.Stream

import practice.ch5.unfold

// 传名函数
sealed trait Stream[+A] {
	// 引入静态方法
	import Stream._
	
	// 未实现的方法可以在混入该特质的类和对象中实现
	def unCons: Option[(A, Stream[A])] =
		this match {
			case Empty => None
			case Cons(h, l) => Some(h(), l())
		}
		
	// 要用 foldRight实现headOption 就要想到如何利用非严格函数去实现,就要在第一步直接短路,因为实际上只需要访问头就知道头是啥了
	// ones.headOption.get 就可以验证正确性
	
	def toList: List[A] =
		unCons match {
			case None => Nil
			case Some((h, t)) => h:: t.toList
		}
	
	def take(n: Int): Stream[A] =
		if (n == 0) Empty
		else unCons match {
			case None => Empty
			case Some((h, t)) => cons(h, t.take(n-1))
		}
	
	def drop(n: Int): Stream[A] =
		if (n == 0 ) this
		else unCons match {
			case None => Empty
			case Some((h,t)) => t.drop(n-1)
		}
		
	def takeWhile(p: A => Boolean): Stream[A] =
		unCons match {
			case None => Empty
			case Some((h, t)) => cons(h, t.takeWhile(p))
		}
		
	def forAll(p: A => Boolean): Boolean =
		unCons match {
			case None => true
			case Some((h, t)) => p(h) && t.forAll(p)
		}
	
	// => B 表示第二个参数是非严格求值
	def foldRight[B] (z: => B)(f: (A, => B) => B): B =
		unCons match {
			case None => z
			case Some((h, t)) => f(h, t.foldRight(z)(f))
		}
	
	def headOption: Option[A] =
		foldRight(None: Option[A]){
			(a, b) => Some(a)
		}
		
	def map[B] (f: A => B): Stream[B] =
		foldRight(Empty: Stream[B]) {
			(a, b) => cons(f(a), b)
		}
		
	def filter(f: A => Boolean): Stream[A] =
		foldRight(Empty: Stream[A]) {
			(a, b) => if(f(a)) cons(a, b) else b
		}
		
	// append 方法参数应该是非严格求值, 如果严格求值会怎样？
	def append[B>: A](n: => Stream[B]): Stream[B] =
		foldRight(n) {
			(a, b) => cons(a, b)
		}
		
	def flatMap[B>: A](f: A => Stream[B]): Stream[B] =
		foldRight(Empty: Stream[B]) {
			(a, b) => f(a).append(b)
		}
	
	// 5.13
	def mapUseUnfold[B] (f: A => B): Stream[B] =
		unfold(this) {
			case Empty => None
			case Cons(h, t) => Some(f(h()), t())
		}
		
	def takeUseUnfold (n: Int): Stream[A] =
		unfold((this, n)) {
			case (Empty, _) => None
			case (_, 0) => None
			case (Cons(h, t), _) => Some(h(), (t(), n-1))
		}
		
	def takeWhileUseUnfold (f: A => Boolean): Stream[A] =
		unfold(this) {
			case Cons(h, t) if f(h()) => Some(h(), t())
			case _ => None
		}
	
	def startWith[B>: A](s: => Stream[B]): Boolean =
		unfold((this, s)) {
			case (_, Empty)	=> None // 遍历完成
			case (Cons(ha, la), Cons(hs, ls)) if ha() == hs() => Some(true, (la() ,ls()))
			case _ => Some(false, (Empty, Empty)) //其他情况，直接终止
		}.forAll(x => x)
	
	def tails: Stream[Stream[A]] =
		unfold(this) {
			case Empty => None
			case Cons(h, t) => Some(Cons(h, t), t())
		}
		
	// 应该不能用 unfold 实现 ，因为 unfold 要从左向右构造 stream, 而这里想要复用数据，那必须从右到左构造stream
	def scanRight[B] (z: => B)(f: (A, => B) => B): Stream[B] =
		unCons match {
			case None => cons(z, Empty)
			case Some((h, t)) => {
				val tmp = t.scanRight(z)(f)
				cons(f(h,tmp.headOption.get), tmp)
			}
		}
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	def cons[A] (hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}
	
	def empty[A]: Stream[A] = Empty
	
	def apply[A] (as: A*): Stream[A] =
		if (as.isEmpty) empty: Stream[A]
		else cons(as.head, apply(as.tail: _*))
	
}