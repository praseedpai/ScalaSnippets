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

        def flatMap[B](f: A => Sequence[B]): Sequence[B] = {
		val mapRes: Sequence[Sequence[B]] = map(f) //map
		flattenLike(mapRes) //flatten
	}

	private def flattenLike[B](seqOfSeq: Sequence[Sequence[B]]): Sequence[B] = {
		var xs = scala.collection.mutable.ArrayBuffer[B]()
		for (listB: Sequence[B] <- seqOfSeq) {
			for (e <- listB) {
				xs += e
			}
		}
		Sequence(xs: _*)
	}
}


object SequenceTest_Step03{


def main(args: Array[String]) {
   case class Person(name: String)
	val myFriends = Sequence(
		Person("Adam"),
		Person("David"),
		Person("Frank")
	)
	val adamsFriends = Sequence(
	        Person("Nick"),
                Person("David"),
                Person("Frank")
        )

   val mutualFriends = for {
	myFriend <- myFriends // generator
	adamsFriend <- adamsFriends // generator
	if (myFriend.name == adamsFriend.name)
    } yield myFriend
    mutualFriends.foreach(println)
        
       
}

}