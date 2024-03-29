
/**
 * https://eed3si9n.com/revisiting-implicits-without-import-tax
 * https://eed3si9n.com/implicit-parameter-precedence-again
 * https://eed3si9n.com/herding-cats/import-guide.html
 * https://meta.plasm.us/posts/2019/09/30/implicit-scope-and-cats/
 *
 * https://stackoverflow.com/questions/5598085/where-does-scala-look-for-implicits/5598107#5598107
 *
 * https://stackoverflow.com/questions/63993620/implicit-categories-in-type-class-resolution
 *
 * https://scalac.io/blog/typeclasses-in-scala/ *****
 */




import cats.Show

//If you remove this definition
//The instance is fetch from the implicit scope.
//Here that is the companion Object

/*implicit val myshowInt = new cats.Show[Int] {
  override def show(t: Int) = t.toString
}*/

cats.Show.apply[Int]

/**
 * If you had the implicit instance myshowInt uncommented,
 * then the scope of aShowInt and myshowInt would be the same i.e. explicit scope.
 * Hence you would have an ambiguous implicit issue i.e. clash within scope
 */
def printImplicit(e: Int)(implicit aShowInt: Show[Int]): Unit = {
  Show[Int].show(e)
  //import cats.syntax.all._
  //e.show
}

object myImplicits {
  implicit val s2: String = "hello, world\n"
}
object Y {
  implicit val n: Int = 17
  trait T {
    implicit val i: Int = 17
    implicit def t: T   = ???
  }
  object X extends T {
    implicit val n: Int = 42
    /**
     * This Works because the implicit values have the same name
     * Otherwise there would be a clash of scope
     * If you change s to s1 above or here, the lexical scope will Clash
     */
    implicit val s: String = "hello, world\n"
    //Same thing if it is an import btw
    //myImplicits._

    def f(implicit s: String) = {

      implicitly[String] * implicitly[Int]
    }

    override def t: T = ???
    //def g = implicitly[T]
  }
}
import Y.X._
f

/**
 * Implicit scope resolution test
 */

object TypeClasses {

  trait Display[A] {
    def display(a: A): String
  }

  object Display {
    def apply[A: Display]: Display[A] = implicitly[Display[A]]

    def display[A: Display](a: A): String = Display[A].display(a)

    object Ops {
      implicit class DisplayOps[A: Display](a: A) {
        def display: String = Display[A].display(a)
      }
    }
    import DataTypes.MyInt
    implicit val DisplayMyInt: Display[MyInt] = _.int.toString
  }
}

object DataTypes {

  import TypeClasses.Display

  case class MyInt(int: Int)
  object MyInt {
    //implicit val MyIntDisplay: Display[MyInt] = _.int.toString
  }

}

import DataTypes.MyInt
//import DataTypes.MyInt._
import TypeClasses.Display.Ops._

MyInt(10).display

/**
 * https://meta.plasm.us/posts/2019/09/30/implicit-scope-and-cats/#orphan-instances
 *
 * Where implicit scope doesn't work is when we have two libraries that have no dependency in either direction.
 * For example, the Squants library doesn't depend on Circe, and Circe doesn't depend on Squants,
 * so if we wanted to provide an io.circe.Decoder instance for squants.space.Length,
 * we wouldn't be able to define it in either the Decoder or Length companion objects.
 * We'd have to fall back to a separate compatibility module that depended on both,
 * with instances that the user would have to import into lexical scope.
 *
 *
 * Bottom line is if you have two candidate possible in the same scope (explicit or implicit scope), you have a conflict.
 * Otherwise priority is explicit then implicit scope.
 * The rule of shadowing only apply to situation where in the same scope you have candidate with the same name.
 *
 * Category 1 implicit are implicit defined at the same level as their syntax or object interface.
 * Category 2 are (basically when following best practices) those defined in the companion object of the type class and separated from the syntax or object interfaces.
 *
 *
 * */
