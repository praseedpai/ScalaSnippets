import scala.annotation.tailrec

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