package gen

import rng.RNG
import Prop._


case class  Prop(run: (TestCases, RNG) => Result) {
	def check: Boolean = ???
	
	def &&(p: Prop): Prop = Prop(
		(tc, rng) => run(tc, rng) match {
			case Passed => p.run(tc, rng)
			case x => x
		}
	)
	
	def ||(p: Prop): Prop = Prop(
		(tc, rng) => run(tc, rng) match {
			case Falsified(_, _) => p.run(tc, rng)
			case x => x
		}
	)
}

object Prop {
	type FailedCase = String
	type SuccessCount = Int
	type TestCases = Int
	
	sealed trait Result extends Product with Serializable {
		def isFalsified: Boolean
	}
	
	case object Passed extends Result {
		def isFalsified = false
	}
	
	case class Falsified(failure: FailedCase,
	                     successes: SuccessCount) extends Result {
		def isFalsified: Boolean = true
	}
	
	def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
		(n, rng) => {
			randomStream(as)(rng).zip(Stream.from(0)).take(n).map{
				case(a, i) => try {
					if (f(a)) Passed else Falsified("",i)
				} catch {case e: Exception => Falsified(buildMsg(a, e), i) }
			}.find(_.isFalsified).getOrElse(Passed)
		}
	}
	
	def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
		unfold(rng)(rng => Some(g.Sample.run(rng)))
	}
	
	def unfold[A, S] (z: S)(f: S => Option[(A, S)]): Stream[A] = {
		f(z) match {
			case None => Stream.Empty
			case Some((a, s)) => Stream.cons(a, unfold(s)(f))
		}
	}
	
	def buildMsg[A](s: A, e: Exception): String =
		s"test case: $s\n" +
		s"generated an exception: ${e.getMessage}\n" +
		s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
		
	
}

