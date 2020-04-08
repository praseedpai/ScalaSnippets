import scala.annotation.tailrec

object UpperExample {
class Upper {
        
        def factorial_r(i:Int) : Long = {
              if ( i < 2 ) 1
              else i*factorial_r(i-1)
        }
        
	def factorial(i: Int): Long = {
                @tailrec
		def fact(i: Int, accumulator: Int): Long = {
		if (i <= 1) accumulator
		else fact(i - 1, i * accumulator)
		}
		fact(i, 1)
	}

}



def main(args: Array[String]) {
        val n = new Upper
        (0 to 5) foreach ( i => println(n.factorial_r(i)) )
        (1 to 10) filter (_ % 2 == 0) map (_ * 2) reduce (_ * _) 
        val value  = (1 to 10).filter (_ % 2 == 0).map (_ * 2).reduce (_ * _)
        println(value)
    }

}