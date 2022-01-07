package gen

import rng.{RNG, SimpleRNG}
import state._

case class Gen[A](Sample: State[RNG,A]) {
	def map[B](f: A => B): Gen[B] =
		Gen(this.Sample.map(f))
	
	def flatMap[B](f: A => Gen[B]): Gen[B] =
		Gen.join(this.map(f))
	
	def listOfN(size: Gen[Int]): Gen[List[A]] =
		size.flatMap(sz => Gen.listOfN(sz, this))
		
	def unsized: SGen[A] =
		SGen(a => this)
}

object Gen {
	// 8.1
	/**
	 * l.reverse().sum = l.sum
	 * List.fill(n)(x).sum = n * x
	 */
	
	/**
	 * l.contain(l.max) && l.forAll(_ => _ <= l.max)
	 */
	
	def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen (State(
			RNG.map(RNG.nonNegativeInt)(_ % (stopExclusive - start) + start)
		))
	
	def unit[A] (a: => A): Gen[A] = Gen(State(
			rng => (a, rng)
		))
	
	def boolean: Gen[Boolean] = Gen(State(
			RNG.map(RNG.nonNegativeInt)(int => if(int%2 == 1) true else false)
		))
	
	def listOfN[A] (n: Int, g: Gen[A]): Gen[List[A]] =
		Gen(State(
				RNG.sequence(
					List.fill(n)(g.Sample.run)
				)
			))
	
	def join[A](g: Gen[Gen[A]]): Gen[A] =
		Gen(State{
			rng => {
				val (g1, r1) = g.Sample.run(rng)
				g1.Sample.run(r1)
			}
		})
		
	def union[A] (g1: Gen[A], g2: Gen[A]): Gen[A] =
		boolean.flatMap(bool => if(bool) g1 else g2)
		
	def weighted[A] (g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
		Gen(State(RNG.double)).flatMap(d => {
			if(d < g1._2/(g1._2+g2._2)) g1._1 else g2._1
		})
}

case class SGen[A] (forSize: Int => Gen[A])