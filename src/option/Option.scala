package option

sealed trait Option[+A] {
	def isEmpty: Boolean
	def map[B] (f: A => B): Option[B] =
		this match {
			case Some(x) => Some(f(x))
			case None => None
		}
		
	def flatMap[B](f: A => Option[B]): Option[B] =
		this match {
			case Some(x) => f(x)
			case None => None
		}
	
	// 为什么用 => B 而不是 B？ 在纯函数里这两个行为是一样的，但是 B 会被立即求值，而 => B 只会在需要的时候求值，可以提高运行效率
	def getOrElse[B >: A] (default: => B): B =
		this match {
			case Some(a) => a
			case None => default
		}
		
	def orElse[B >: A] (ob: => Option[B]): Option[B] =
		this match {
			case None => ob
			case Some(_) => this
		}
	
	def filter(f: A => Boolean): Option[A] =
		this match {
			case None => None
			case Some(x) => if(f(x)) this else None
			// case Some(x) if(f(x)) => this
			// case _ => None
		}
}

case class Some[+A] (value: A) extends Option[A] {
	def isEmpty = false
}
case object None extends Option[Nothing] {
	def isEmpty = true
}

