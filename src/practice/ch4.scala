package practice

import option._

object ch4 {
	// 4.1
	// Option/Option.scala
	
	def mean (xs: Seq[Double]): Option[Double] = {
		if(xs.isEmpty)  None
		else            Some(xs.sum/xs.length)
	}
	
	def variance (xs: Seq[Double]): Option[Double] = {
		
		mean(xs) match {
			case Some(m) => Some(xs.map(x => (x - m) * (x - m)).sum)
			case None => None
		}
	}
	
	
}
