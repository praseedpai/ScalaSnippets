//////////////////////////
// A Simple Scala Program to demonstrate Stack
// To run:
//    scalac MainObject2.scala
//    scala -classpath . MainObject
//
object MainObject2{ 
    //------------ A Simple Stack using List<A>
    class Stack[A] {
      private var elements: List[A] = Nil
      def push(x: A) { elements = x :: elements }
      def peek: A = elements.head
      def pop(): A = {
         val currentTop = peek
         elements = elements.tail
         currentTop
      }
    }

    class Fruit
    class Apple extends Fruit
    class Banana extends Fruit

   
    //---- EntryPoint 
    def main(args:Array[String]){  
        val stack = new Stack[Int]
	stack.push(1)
	stack.push(2)
	println(stack.pop)  // prints 2
	println(stack.pop)  // prints 1

        val stack2 = new Stack[Fruit]
        val apple = new Apple
        val banana = new Banana

        stack2.push(apple)
        stack2.push(banana)
    }  
}  