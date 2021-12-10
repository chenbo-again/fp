package practice

object ch2 {
	// 2.2
	def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
		var i = 1
		while (i < as.length) {
			if (ordered(as(i - 1), as(i)))
				return false
			i = i + 1
		}
		true
	}
	
	// 2.3
	def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
		(a: A) => ((b: B) => (f(a, b)))
	}
	
	// 2.4
	def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
		(a: A, b: B) => f(a)(b)
	}
	
	// 2.5
	def compose[A, B, C](f: B => C, g: A => B): A => C = {
		a: A => f(g(a))
	}
}
