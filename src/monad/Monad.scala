package monad

import rng.RNG
import state.State

import scala.language.higherKinds

object Monad {
	// F[_]是类型构造器，这里指有函子性质的类型构造器
	trait Functor[F[_]] {
		def map[A,B](fa: F[A])(f: A => B): F[B]
		
		def distribute[A,B] (f: F[(A,B)]):(F[A], F[B]) =
			(map(f)(_._1), map(f)(_._2))
			
		def codistribute[A,B] (e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
			case Left(fa) => map(fa)(Left(_))
			case Right(fb) => map(fb)(Right(_))
		}
	}
	
	trait Monad[F[_]] extends Functor[F] {
		def unit[A](a: A): F[A]
		
		def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] =
			join(map(fa)(f))
		
		def map[A,B](fa: F[A])(f: A => B): F[B] =
			flatMap(fa)(a => unit(f(a)))
		
		def map2[A,B,C] (fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
			flatMap(fa)(a => map(fb)(b => f(a,b)))
			
		def sequence[A](lma: List[F[A]]): F[List[A]] =
			lma.foldLeft(unit(Nil: List[A]))((x, y) => map2(x, y)((a, b) => b:: a))
			
		def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
			sequence(la.map(f))
			
		def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
			sequence(List.fill(n)(ma))
			
		def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
			map2(fa,fb)((_, _))
			
		def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ???
		
		def compose[A,B,C] (f: A => F[B], g: B => F[C]): A => F[C] =
			a => flatMap(f(a))(g)
			// a: A => join(map(f(a))(g))
		
		// 常函数直接接受一个 Unit 就行了
		def _flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] =
			compose((_: Unit) => fa, f)(())
//			def dull(x: Int): F[A] = fa
//			compose(dull, f)(1)
		
		def join[A](mma: F[F[A]]): F[A] =
			flatMap(mma)(ma => ma)
		
		//
	
	}
	
	val optionMonad: Monad[Option] = new Monad[Option] {
		def unit[A](a: A): Option[A] = Some(a)
		
		override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
			fa match {
				case None => None
				case Some(e) => f(e)
			}
			
		// flatMap(Some(v))(unit) = x
		// flatMap(unit(x))(f) = f(y)
	}
	
	val listMonad: Monad[List] = new Monad[List] {
		def unit[A](a: A): List[A] = List(a)
		
		// 可以利用一个递归实现
		override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
			fa.flatMap(f)
		
	}
	
	case class Id[A](value: A)
	
	val idMonad: Monad[Id] = new Monad[Id] {
		def unit[A](a: A): Id[A] = Id(a)
		
		override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa.value)
	}
	
	type IntState[A] = State[Int, A]
	
	object InstateMonad extends Monad[IntState] {
		def unit[A](a: A): IntState[A] =
			State(s => (a, s))
		
		override def flatMap[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] =
			fa.flatMap(f)
	}
	
	
	//然而这不能编译通过
//	val stateMonad: Monad[State[RNG,_]] = new Monad[State[RNG,_]] {
//		def unit[A](a: A): State[RNG, _][A] = ???
//
//		def flatMap[A, B](fa: State[RNG, _][A])(f: A => State[RNG, _][B]): State[RNG, _][B] = ???
//	}


}
