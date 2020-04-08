import scala.annotation.tailrec
import scala.language.higherKinds

trait SemiGroup {

  def combine(x: A, y: A): A
}

trait Monoid[A] {
	def combine(x: A, y: A): A
	def empty: A
}



trait Functor[F[_]] { 
def map[A, B](fa: F[A])(f: A => B): F[B] 
}


trait Monad[F[_]] extends Functor[F] {
	def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
	def pure[A](x: A): F[A]
	def map[A, B](fa: F[A])(f: A => B): F[B] =
		flatMap(fa)(f andThen pure)
}


object SeqF extends Functor[Seq] { 
	def map[A, B](seq: Seq[A])(f: A => B): Seq[B] = seq map f
}


class OptionF extends Functor[Option] {
 def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt map f
}


object ReverseAList extends App {
	implicit val strMonoid = new Monoid[String] {
		def combine(x: String, y: String): String = x + y
		def empty: String = ""
	}

	implicit val intMonoid = new Monoid[Int] {
		def combine(x: Int, y: Int): Int = x + y
		def empty: Int = 0
	}

        implicit def listMonoid[A]: Monoid[List[A]] =
		new Monoid[List[A]] {
			def combine(x: List[A], y: List[A]): List[A] = ???
			def empty: List[A] = ???
	}

        implicit val optionMonad = new Monad[Option] {
		def pure[A](x: A): Option[A] = Some(x)
		def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
			case Some(x) => f(x)
			case None => None
		}
        }
        def sum[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.empty)(m.combine)

        println(sum(List("Monoids", " are", " cool"))) // "Monoids are cool"
	println(sum(List(1,2,3))) // 6
	println(sum(List(List(1,2),List(3,4)))) // List(1,2,3,4)
}