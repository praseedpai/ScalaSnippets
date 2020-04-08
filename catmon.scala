import scala.annotation.tailrec
import scala.language.higherKinds
trait Monoid[A] {
def combine(x: A, y: A): A
def empty: A
}


object catmon extends App {

implicit val strMonoid = new Monoid[String] {
def combine(x: String, y: String): String = x + y
def empty: String = ""
}

implicit val strMonoid = new Monoid[String] {
def combine(x: String, y: String): String = x + y
def empty: String = ""
}

println(strMonoid.combine("Monoids are ", "great"))
println(strMonoid.combine("Hello", strMonoid.empty))

}