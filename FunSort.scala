/////////////////////////////////
//
//
//
//
//



object HelloWorld {

   /*
	def sort(xs: Array[Int]): Array[Int] = {
		if (xs.length <= 1) xs
		else {
			val pivot = xs(xs.length / 2)
			Array.concat(
				sort(xs filter (pivot >)),
					xs filter (pivot ==),
					sort(xs filter (pivot <)))
		}
	}
    */

def sort3(xs: Array[Int]): Array[Int] = {
		if (xs.length <= 1) xs
		else {
			val pivot = xs(xs.length / 2)
			Array.concat(
				sort3(xs.filter (_ < pivot)),
					xs.filter (_ == pivot),
					sort3(xs.filter(_ > pivot)))
		}
	}

        def sort2(xs: List[Int]): List[Int] = {
		if (xs.length <= 1) xs
		else {
			val pivot = xs(xs.length / 2)
			List.concat(
				sort2(xs.partition(_ < pivot)._1),
					xs.partition(_ == pivot)._1,
				sort2(xs.partition(_ > pivot)._1))
		}
	}

def main(args: Array[String]) {
      println("Hello, world!")
      var a = List( 1,-2, 3,-10,-22,7);

      a = sort2(a);

      for( vas <- a )
         println(vas)

      println("======================================")

      var a2 = Array( 1,-2, 3,-10,-22,7);

      a2 = sort3(a2);

      for( vas <- a2 )
         println(vas)
      println("Hello, world!")
    }
  }