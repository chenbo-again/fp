package state

import scala.annotation.tailrec
import scala.language.postfixOps

case class State[S,+A] (run: S => (A, S)) {
	import State._
	
	def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
		val (a, s1) = run(s)
		f(a).run(s1)
	})
	
	def map[B](f: A => B): State[S, B] =
		flatMap(a => unit(f(a)))
	
	def map2[B,C](b: State[S,B]) (f: (A,B) => C): State[S,C] =
		flatMap(a => {
			b.map(x => f(a,x))
		})
	
	
	
	
}

object State {
	
	def unit[S,A](a: A): State[S,A] = State(a -> )
	
	def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
		@tailrec
		def go(xs: List[State[S,A]], s: S, a: List[A]): (List[A],S) = {
			xs.headOption match {
				case None => (a, s)
				case Some(x) => {
					val (a1, s1) = x.run(s)
					go(xs.tail, s1, a1::a)
				}
			}
		}
		
		State(go(fs, _, Nil: List[A]))
	}
	
	
}