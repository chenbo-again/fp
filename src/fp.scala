import practice.ch6._
import rng._

object fp extends App{
	val v = RNG.nonNegativeLessThan(10)(SimpleRNG(42))
	println(v)
	
}