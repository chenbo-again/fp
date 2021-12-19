package par

import java.util.concurrent._
import scala.concurrent.duration.TimeUnit

object Par {
	type Par[A] = ExecutorService => Future[A]
	def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
	
	// 封装单个值，事实上不开辟一个新线程
	private case class UnitFuture[A] (get: A) extends Future[A] {
		def isDone = true
		def get(timeout: Long, units: TimeUnit): A = get
		def isCancelled = false
		def cancel(evenIfRunning: Boolean): Boolean = false
	}
	
	// 支持超时设置是简单的,使用Future的另外一个有超时限制的get就可以
	// UnitFuture(f(af.get(timeout: Long, unit: TimeUnit), bf.get(timeout: Long, unit: TimeUnit))即可
	def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
		es => {
			val (af, bf) = (a(es), b(es))
			UnitFuture(f(af.get, bf.get))
		}
	
	
	// 分流，描述计算
	// submit() 方法会将一个 Callable 或 Runnable 任务提交给 ExecutorService 并返回 Future 类型的结果。
	def fork[A](a: => Par[A]): Par[A] = {
		es => es.submit(new Callable[A] {
			def call = a(es).get
		})
	}
	
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
	
	def run[A](a: Par[A])(es: ExecutorService): Future[A] = a(es)
	
	def asyncF[A,B] (f: A => B): A => Par[B] = a => lazyUnit(f(a))
	
	def map[A,B] (a: Par[A]) (f: A => B): Par[B] =
		map2(a, unit(())) ((x, _) => f(x))
	
	// 将一系列并行运算组合成一个并行运算，这个并行运算返回一个列表
	// 便于数据处理，运行时其实还是开很多线程并行做的
	def sequence[A] (ps: List[Par[A]]): Par[List[A]] =
		ps.foldRight(unit(Nil: List[A]))((x, y) => map2(x, y)(_:: _))
	
	// 对一个列表中每一个元素并行执行 f
	def parMap[A,B] (ps: List[A]) (f: A => B): Par[List[B]] =
		sequence(ps.map(
			a => asyncF(f)(a)
		))
		
	def parFilter[A] (as: List[A]) (f: A => Boolean): Par[List[A]] = {
		// 并行构造一系列列表
		val pars = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
		map(sequence(pars))(_.flatten)
	}
	
	def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] =
		es => {
			if(run(cond)(es).get) t(es)
			else f(es)
		}
		
	def choiceN[A] (cond: Par[Int]) (ps: List[Par[A]]): Par[A] =
		es => ps(run(cond)(es).get)(es)
		
	def choiceMap[K,V] (key: Par[K]) (choices: Map[K, Par[V]]): Par[V] =
		es => choices(run(key)(es).get)(es)
		
	def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] =
		es => choices(run(pa)(es).get)(es)
	
	def choiceByChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] =
		chooser(cond)(bool => if(bool) t else f)
	
	def choiceNByChooser[A] (cond: Par[Int]) (ps: List[Par[A]]): Par[A] =
		chooser(cond)(ps(_))
		
	def join[A] (a: Par[Par[A]]): Par[A] =
		es => run(a)(es).get()(es)
		
	def flatMap[A,B] (a: Par[A]) (f: A => Par[B]): Par[B] =
		join(map(a)(f))
		
	
}
