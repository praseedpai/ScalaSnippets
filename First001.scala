import scala.util.{Try, Success, Failure}


object Upper {
  def main(args: Array[String]) = {
    
    def accumulate(xs: List[Int]): Int = {
       xs match {
          case x :: tail => x + accumulate(tail) 
          case Nil => 0 
       }
    }
   
    def average(x: Array[Double]): Double = x.sum / x.length
   
    val temp = Try(args.map(x =>   x.toDouble ).sum)

    println(temp)

    
    /*
    temp match {
      case Success(v) =>
           // println(average(v))
           println(v.reduceLeft((x,y) => x * y))
           println(v.foldLeft(0.0)((x,y) => x + y))
      case Failure(e) =>
       
        println("Info from the exception: " + e.getMessage)
       
    } */
    
    println("")
  }
}