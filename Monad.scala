import scala.annotation.tailrec
import scala.language.higherKinds

trait SemiGroup[A] {

  def combine(x: A, y: A): A
}

trait Monoid[A] extends SemiGroup[A] {
	
	def empty: A
}

trait Functor[F[_]] { //
	def map[A, B](fa: F[A])(f: A => B): F[B] 
}

trait Monad[M[_]] {                                                // <1>
  def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]                  // <2>
  def unit[A](a: => A): M[A]                                       // <3>

  // Some common aliases:                                             <4>
  def bind[A,B](fa: M[A])(f: A => M[B]): M[B] = flatMap(fa)(f)
  def >>=[A,B](fa: M[A])(f: A => M[B]): M[B] = flatMap(fa)(f)
  def pure[A](a: => A): M[A] = unit(a)
  def `return`[A](a: => A): M[A] = unit(a)    // backticks to avoid keyword
}

object SeqM extends Monad[Seq] {
  def flatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq flatMap f
  def unit[A](a: => A): Seq[A] = Seq(a)
}

object OptionM extends Monad[Option] {
  def flatMap[A, B](opt: Option[A])(f: A => Option[B]):Option[B]= opt flatMap f
  def unit[A](a: => A): Option[A] = Option(a)
}

object SeqF extends Functor[Seq] {                                   // <3>
  def map[A, B](seq: Seq[A])(f: A => B): Seq[B] = seq map f
}

object OptionF extends Functor[Option] {
  def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt map f
}

object FunctionF {                                                   // <4>
  def map[A,A2,B](func: A => A2)(f: A2 => B): A => B = {             // <5>
    //Type Lambda can be replaced with alias of function
    // type C[T] = A => T
    val functor = new Functor[({type L[T] = A => T})#L] {            // <6>
      def map[A3,B2](func: A=>A3)(f: A3 => B2): A => B2 = (a: A) => f(func(a))
    }
    functor.map(func)(f)                                             // <7>
  }
}

object ReverseAList4 extends App {
	val seqf: Int => Seq[Int] = i => 1 to i
	val optf: Int => Option[Int] = i => Option(i + 1)

	SeqM.flatMap(List(1,2,3))(seqf)             // Seq[Int]: List(1,1,2,1,2,3)
	SeqM.flatMap(List.empty[Int])(seqf)         // Seq[Int]: List()

	OptionM.flatMap(Some(2))(optf)              // Option[Int]: Some(3)
	OptionM.flatMap(Option.empty[Int])(optf)    // Option[Int]: None 
}