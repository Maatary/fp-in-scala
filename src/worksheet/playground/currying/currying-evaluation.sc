/**
 *  Function currying
 *
 *  What I was trying to understand is how supplying the first argument ensure
 *  that you get a function where that parameter is already evaluated in the body of the function returned.
 *  Review the use of state monad update function &  Haskell Weak Head Normal Form.
 */


val f = (a: String) => (b: String) => (c :Int) => b match {
  case "hello" => println("it's hello"); s"hello $a"
  case _ => println("it's not hello"); s"good by $a"
}


//val f1 = f("Maatari")

//f1("hello")(1)

val partialf = f("Maatari")("hello")




val h = (a: String) => (b: String) => b match {
  case "hello" =>  (c: Int)  => { s"hello $a you are ${c.toString} years old"}
  case _       =>  (c: Int)  => s"bye $a you are ${c.toString} years old"
}


def func(a:  String)(b: String) = a + b

func("2")(_)







