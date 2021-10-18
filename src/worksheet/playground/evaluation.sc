


val f = (a: String) => (b: String) => (c :Int) => b match {
  case "hello" => println("it's hello"); s"hello $a"
  case _ => println("it's not hello"); s"good by $a"
}

f("Maatari")("hello")(1)


val h = (a: String) => (b: String) => b match {
  case "hello" =>  (c: Int)  => { s"hello $a you are ${c.toString} years old"}
  case _       =>  (c: Int)  => s"bye $a you are ${c.toString} years old"
}


def func(a:  String)(b: String) = a + b

func("2")(_)







