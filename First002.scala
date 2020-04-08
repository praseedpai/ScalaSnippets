


object Upper {
  def main(args: Array[String]) = {
    
    def accumulate(xs: List[Int]): Int = {
       xs match {
          case x :: tail => x + accumulate(tail) 
          case Nil => 0 
       }
    }
   
   
    val temp = args.map(x =>   x.toInt )
    
    println(accumulate(temp.toList))
      
    println("")
  }
}