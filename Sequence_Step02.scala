import scala.annotation.tailrec

case class Sequence[A](initialElems: A*) {
	private val elems = scala.collection.mutable.ArrayBuffer[A]()
	// initialize
	elems ++= initialElems

	def map[B](f: A => B): Sequence[B] = {
		val abMap = elems.map(f)
		new Sequence(abMap: _*)
	}

	def foreach(block: A => Unit): Unit = {
		elems.foreach(block)
	}

        def withFilter(p: A => Boolean): Sequence[A] = {
		val tmpArrayBuffer = elems.filter(p)
		Sequence(tmpArrayBuffer: _*)
	}
}


object SequenceTest_Step02{


def main(args: Array[String]) {
    val ints = Sequence(1,2,3)

    // (1) works because `foreach` is defined
	for (p <- ints ) println(p)
    // (2) `yield` works because `map` is defined
	val res = for {
		i <- ints
		if i > 2
	} yield i*2
	res.foreach(println) // verify the result
        
       
}

}