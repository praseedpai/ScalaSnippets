
//------------ The below line will hide the Standard Library Option/Either
import scala.{Option => _, Either => _, _}




sealed trait Option[+A]  {
 def map[B](f: A => B): Option[B] = this match {
    
     case None => None
    case Some(a) => Some(f(a))
 
 }

  

 def getOrElse[B>:A](default: => B): B = this match {
    
     case None => default
    
     case Some(a) => a
  
 }

  

  def flatMap[B](f: A => Option[B]): Option[B] =
    
           map(f) getOrElse None

   

   /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
  
       case None => None
    
       case Some(a) => f(a)
  
  }

 def orElse[B>:A](ob: => Option[B]): Option[B] =
   
      this map (Some(_)) getOrElse ob

 
 def filter(f: A => Boolean): Option[A] = this match {
    
      case Some(a) if f(a) => this
    case _ => None
  
 }
  
  

 def filter_1(f: A => Boolean): Option[A] =
   
     flatMap(a => if (f(a)) Some(a) else None)

}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


//-------------------------
//-------------------------
sealed trait Either[+E,+A] {
 
   def map[B](f: A => B): Either[E, B] = 
   this match {
 
        case Right(a) => Right(f(a))
     
        case Left(e) => Left(e)
  
   }
   


   def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
   this match {
 
       case Left(e) => Left(e)
    
       case Right(a) => f(a)
  
   }
 


   def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
   this match {
 
       case Left(_) => b
     
       case Right(a) => Right(a)
   
   }
 

   def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
   Either[EE, C] = 
       for { a <- this; b1 <- b } yield f(a,b1)


}





case class Left[+E](get: E) extends Either[E,Nothing]

case class Right[+A](get: A) extends Either[Nothing,A]



object Map {

       def mean(xs: Seq[Double]): Option[Double] =
    
                 if (xs.isEmpty) None
    
                 else Some(xs.sum / xs.length)

  

       def variance(xs: Seq[Double]): Option[Double] =
   
           mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


       def mean2(xs: IndexedSeq[Double]): Either[String, Double] =
		if (xs.isEmpty)
			Left("mean of empty list!")
		else
			Right(xs.sum / xs.length)

       def Try[A](a: => A): Option[A] =
		try Some(a)
		catch { case e: Exception => None }

       def Try2[A](a: => A): Either[Exception, A] =
    
           try Right(a)
    
           catch { case e: Exception => Left(e) }

  

      def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  
          es match {
     
            case Nil => Right(Nil)
     
            case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    
         }
        
	def main (args: Array[String]): Unit = {

                 // val temp = Try(args.map(x =>   x.toDouble ).sum)

 
                 val temp = Try(args.map(x =>   x.toDouble ))

   		 println(temp)

                 temp match {
      			case Some(v) =>
           			 // println(average(v))
          			// println(v.reduceLeft((x,y) => x * y))
         		         println(v.foldLeft(0.0)((x,y) => x + y))
     			 case None =>
       
       				 println("Info from the exception: ")
       
   		 } 

                 val temp2 = Try2(args.map(x =>   x.toDouble ).sum)
                 println(temp2)

		 temp2 match {
      			case Right(v) =>
           			 // println(average(v))
          			 println(v)
         		       
     			 case Left(v) =>
       
       				 println("Info from the exception: " + v )
       
   		 } 

	}
}