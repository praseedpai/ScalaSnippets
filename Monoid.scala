import scala.annotation.tailrec
import scala.language.higherKinds

trait SemiGroup[A] {

  def combine(x: A, y: A): A
}

trait Monoid[A] extends SemiGroup[A] {
	
	def empty: A
}



object ReverseAList3 extends App {
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
			def combine(x: List[A], y: List[A]): List[A] = x ++ y
			def empty: List[A] = List.empty
	}

    
        def sum[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.empty)(m.combine)

        println(sum(List("Monoids", " are", " cool"))) // "Monoids are cool"
	println(sum(List(1,2,3))) // 6
	println(sum(List(List(1,2),List(3,4)))) // List(1,2,3,4)
}