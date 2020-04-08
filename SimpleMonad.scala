import scala.annotation.tailrec


class HolderofInt[Int](value: Int) {
	def map(f: Int => Int): HolderofInt[Int] = {
		val newValue = f(value)
		new HolderofInt(newValue)
	}
	def flatMap(f: Int => HolderofInt[Int]): HolderofInt[Int] = f(value)
	override def toString = value.toString
}

class Holder[A] private (value: A) {
	def map[B](f: A => B): Holder[B] = {
		val newValue = f(value)
		new Holder(newValue)
	}
	def flatMap[B](f: A => Holder[B]): Holder[B] = {
		val newValue = f(value)
		newValue
	}
	override def toString = value.toString
}

object Holder{
	def apply[A](value: A): Holder[A] = new Holder(value)
}


object PartiallyDefined {


def main(args: Array[String]) {
        
        val inverse: PartialFunction[Double,Double] = {
           case d if d != 0.0 => 1.0 / d
        }

        def add ( a:Int , b:Int ) = a + b 

        println(inverse(10))
        println(add(20,40))
    }

}