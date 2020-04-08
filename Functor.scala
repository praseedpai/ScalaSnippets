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

object ReverseAList extends App {
	val fii: Int => Int = i => i * 2
        val fii2: Int => Int = i => i + 3
	val fid: Int => Double = i => 2.1 * i
	val fds: Double => String = d => d.toString


	val temp = SeqF.map( 
                       SeqF.map(List(1,2,3,4))(fii)) (fii2)
	val temp2 = 
                       SeqF.map(List(1,2,3,4))(fii compose fii2)
        println(temp)
         println(temp2)

	SeqF.map(List.empty[Int])(fii) // Seq[Int]: List()
	OptionF.map(Some(2))(fii) // Option[Int]: Some(4)
	OptionF.map(Option.empty[Int])(fii) // Option[Int]: None
	val fa = FunctionF.map(fid)(fds) //
	fa(2) // String: 4.2
	// val fb = FunctionF.map(fid)(fds)
	val fb = FunctionF.map[Int,Double,String](fid)(fds)
	fb(2)
	val fc = fds compose fid //
	fc(2)
}