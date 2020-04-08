
object UpperExample {
class Upper {
	def upper(strings: String*): Seq[String] = {
		strings.map((s:String) => s.toUpperCase())
	}
        def upper2(strings: String*) = strings.map(_.toUpperCase())
}



def main(args: Array[String]) { // String [] args
        val up = new Upper()
	println(up.upper("Hello", "World!"))
        println(up.upper2("Hello", "World!"))
      //  args.map(_.toUpperCase()).foreach(printf("%s ",_))
      //  val output = args.map(_.toUpperCase()).mkString(" ")
      //  println(output)
    }

}