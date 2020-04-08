import scala.annotation.tailrec

object UpperExample {




def main(args: Array[String]) {
        def cat1(s1: String)(s2: String) = s1 + s2
        val temp = cat1("Hello") _
        println(temp(" World"))
        println(cat1("Hello ")("World!"))

        val inverse: PartialFunction[Double,Double] = {
           case d if d != 0.0 => 1.0 / d
        }

        println(inverse(10))
    }

}