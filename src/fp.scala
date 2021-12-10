import practice.ch5._
import datastructures.Stream._

object fp extends App{
	def test[A] (s: Stream[A]): Unit = {
		println(s.take(10).toList)
	}
	val s1 = Stream(1, 1, 4, 5, 1, 4)
	// reverse
//	val s2 = s1.foldRight(Empty: Stream[Int]){
//		(a, b) => b.append(a)
//	}
	println(s1.startWith(Stream(1, 1, 4, 2)))
	// test(s2)
}