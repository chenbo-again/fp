package rng

import scala.annotation.tailrec
import scala.language.postfixOps

trait RNG {
	def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
	def nextInt: (Int, RNG) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >> 16).toInt
		
		(n, nextRNG)
	}
}

object RNG extends {
	type Rand[+A] = RNG => (A, RNG)
	
	
	// sugar for _: RNG => (A, _)
	def unit[A](a: A): Rand[A] = a ->
	
	def int: Rand[Int] = _.nextInt
	
	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (n, rng1) = rng.nextInt
		
		if(n < 0)
			(-1-n, rng1)
		else
			(n, rng1)
	}
	
	def double(rng: RNG): (Double, RNG) = {
		val (n, rng1) = nonNegativeInt(rng)
		
		(n.toDouble/ Int.MaxValue.toDouble, rng1)
	}
	
	def intDouble(rng: RNG): ((Int, Double), RNG) = {
		val (d, rng1) = double(rng)
		val (i, rng2) = rng1.nextInt
		
		((i, d), rng2)
	}
	
	def doubleInt(rng: RNG): ((Double, Int), RNG) = {
		val (i, rng1) = rng.nextInt
		val (d, rng2) = double(rng1)
		
		((d, i), rng2)
	}
	
	def double3(rng: RNG): ((Double, Double, Double), RNG) = {
		val (d1, rng1) = double(rng)
		val (d2, rng2) = double(rng1)
		val (d3, rng3) = double(rng2)
		
		((d1, d2, d3), rng3)
	}
	
	def ints(count: Int) (rng: RNG): (List[Int], RNG) = {
		@tailrec
		def go(cnt: Int, rng0: RNG, l: List[Int]): (List[Int], RNG) = {
			if(cnt == 0) {
				(l, rng0)
			} else {
				val (n, rng1) = rng0.nextInt
				go(cnt - 1, rng1, n :: l)
			}
		}
		go(count, rng, Nil)
	}
	
	def map[A,B] (s: Rand[A]) (f: A => B): Rand[B] =
		rng => {
			val (a, rng2) = s(rng)
			
			(f(a), rng2)
		}
	
	def doubleByMap (rng: RNG): (Double, RNG) = {
		map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble) (rng)
	}
	
	def map2[A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] =
		rng => {
			val (a, rng1) = ra(rng)
			val (b, rng2) = rb(rng1)
			
			(f(a, b), rng2)
		}
		
	def sequence[A] (fs: List[Rand[A]]): Rand[List[A]] =
		rng => {
			@tailrec
			def go(lr: List[Rand[A]], rng1: RNG, l: List[A]): (List[A], RNG) = {
				lr.headOption match {
					case None => (l, rng1)
					case Some(ra) => {
						val (a, rng2) = ra(rng1)
						go(lr.tail, rng2, a:: l)
					}
				}
			}
			
			go(fs, rng, Nil)
		}
		
	def intsByS(count: Int)(rng: RNG): (List[Int], RNG) = {
		sequence(List.fill(count) ((x: RNG) => x.nextInt)) (rng)
	}
	
	// 把中间状态暴露出来，方便对中间状态做一些操作，这也使得它更底层
	def flatMap[A,B] (f: Rand[A]) (g: A => Rand[B]): Rand[B] =
		rng => {
			val (a, rng1) = f(rng)
			
			g(a)(rng1)
		}
	
	
	def nonNegativeLessThan(n: Int): Rand[Int] =
		flatMap(nonNegativeInt) {
			a => {
				val mod = a % n
				// It's confusion
				if (a + (n-1) - mod >= 0)
					unit(mod)
				else
					nonNegativeLessThan(n)
			}
		}
		
	// 6.9
	
	def mapByFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
		flatMap(s){
			a => unit(f(a))
		}
		
	def map2ByFlatMap[A,B,C](ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] =
		flatMap(ra) (a => map(rb)(b => f(a, b)))
		
	
}