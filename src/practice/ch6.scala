package practice
import state._
// todo 智商不够用了
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
//	def step(i: Input): State[Machine, (Int,Int)] = {
//		i match {
//			case Coin if locked && candies > 0 => State
//		}
//	}
//	def simulateMachine(input: List[Input]): State[Machine, (Int,Int)] = {
//
//	}
}


object ch6 {
	
}
