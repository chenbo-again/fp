package practice

// List 使用 scala 自带的 List 实现
import datastructures.Tree._
import scala.annotation.tailrec

object ch3 {
	def tail[A](as: List[A]): List[A] =
		as match {
			case Nil => Nil
			case _ :: xs => xs
		}
	
	def setHead[A](as: List[A], head: A): List[A] =
		as match {
			case Nil => Nil
			case _ :: xs => head :: xs
		}
	
	// 丢掉前n个元素
	@tailrec
	def drop[A](l: List[A], n: Int): List[A] =
		n match {
			case 0 => l
			case _ => drop(tail(l), n - 1)
		}
	
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
		if (f(l.head)) tail(l)
		else l
	
	def init[A](l: List[A]): List[A] =
		l match {
			case Nil => Nil
			case _ :: Nil => Nil
			case x :: xs => x :: init(xs)
		}
	
	// TODO: 3.7 我认为可以
	
	// 3.8 不可以，type error
	
	def length[A](l: List[A]): Int =
		l.foldRight(0)((_: A, b: Int) => b + 1)
	
	// 非尾递归，每次递归需要把x值保存起来，可能爆栈
	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
		as match {
			case Nil => z
			case x :: xs => f(x, foldRight(xs, z)(f))
		}
	
	// 尾递归，每次递归只是返回一个调用
	@tailrec
	def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
		as match {
			case Nil => z
			case x :: xs => foldLeft(xs, f(z, x))(f)
		}
	
	// 3.11
	// println(foldLeft(l,0)((x,y) => x+y))
	// println(foldLeft(l,1)((x,y) => x*y))
	// println(foldLeft(l,0)((x,y) => x+1))
	
	def reverse[A](l: List[A]): List[A] =
		foldLeft(l, Nil: List[A])((t, h) => h :: t)
	
	def foldRightUseFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
		foldLeft(reverse(l), z)((a, b) => f(b, a))
	
	// l 被接在 b 前面
	def append[A](l: List[A], b: List[A]): List[A] =
		foldRight(l, b)((x, y) => x :: y)
	
	// 使线性复杂度必须使每个列表只被访问常数次，所以是把访问的列表的列表append到未访问的列表后
	// 前面实现的append是第一个参数被接在第二个参数前面，所以这里应该用foldRight
	def appends[A](ls: List[A]*): List[A] =
		foldRight(ls.toList, Nil: List[A])(append)
	
	def plusOne(l: List[Int]): List[Int] =
		foldRight(l, Nil: List[Int])((x, y) => x + 1 :: y)
	
	def listToStr(l: List[Double]): List[String] =
		foldRight(l, Nil: List[String])((x, y) => x.toString :: y)
	
	def map[A, B](l: List[A])(f: A => B): List[B] =
		foldRight(l, Nil: List[B])((x, y) => f(x) :: y)
	
	// filter(l)(i => i%2==0)
	def filter[A](l: List[A])(f: A => Boolean): List[A] =
		foldRight(l, Nil: List[A])((x, y) => {
			if (f(x)) x :: y
			else y
		})
	
	def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
		appends(map(l)(f): _*)
	
	def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
		flatMap(l)((a: A) => {
			if (f(a)) a :: Nil
			else Nil
		})
	
	def listAdd(l0: List[Int], l1: List[Int]): List[Int] =
		(l0, l1) match {
			case (Nil, _) => Nil
			case (_, Nil) => Nil
			case (x0 :: x0s, x1 :: x1s) => x0 + x1 :: listAdd(x0s, x1s)
		}
	
	def zipWith[A, B, C](l0: List[A], l1: List[B])(f: (A, B) => C): List[C] =
		(l0, l1) match {
			case (Nil, _) => Nil
			case (_, Nil) => Nil
			case (x0 :: x0s, x1 :: x1s) => f(x0, x1) :: zipWith(x0s, x1s)(f)
		}
	
	@tailrec
	def startWith[A](sup: List[A], sub: List[A]): Boolean =
		(sup, sub) match {
			case (_, Nil) => true
			case (suph :: supt, subh :: subt) => if (suph == subh) startWith(supt, subt) else false
			case _ => false
		}
	
	@tailrec
	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
		sup match {
			case Nil => sub == Nil
			case x :: xs => if (startWith(x :: xs, sub)) true else hasSubsequence(xs, sub)
		}
	
	// 树的习题
	def size[A](tr: Tree[A]): Int =
		tr match {
			case Leaf(_) => 1
			case Branch(l, r) => 1 + size(l) + size(r)
		}
	
	def maximum(tr: Tree[Int]): Int =
		tr match {
			case Leaf(x) => x
			case Branch(l, r) => math.max(maximum(l), maximum(r))
		}
	
	def depth[A](tr: Tree[A]): Int =
		tr match {
			case Leaf(_) => 1
			case Branch(l, r) => math.max(depth(l), depth(r)) + 1
		}
	
	def map[A, B](tr: Tree[A])(f: A => B): Tree[B] =
		tr match {
			case Leaf(a: A) => Leaf(f(a))
			case Branch(l, r) => Branch(map(l)(f), map(r)(f))
		}
	
	// 就是深度优先遍历和广度优先遍历，但是树的branch节点由于没有值所以没法做左折叠没意义
	def dFold[A, B](tr: Tree[A], b: B)(f: A => B)(g: (B, B) => B): B =
		tr match {
			case Leaf(a: A) => f(a)
			case Branch(l, r) => g(dFold(l, b)(f)(g), dFold(r, b)(f)(g))
		}
	
	
}
