import scala.util.Random

/**
 * see [[https://pedrorijo.com/blog/scala-enums/ Scala Enumerations]]
 */
val EnumeratumTutorial: Unit = println("Welcome to the Enum Tutorial")

/**
 * see [[scala.Enumeration#Value(int, java.lang.String)]]
 * Basically it create a value with `Value(nextId)`
 *
 * See Also Value(int) and Value(String)
 *
 */
object Weekday0 extends Enumeration {
  val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
}

/**
 * ===Note Trick===
 *
 * When you line up
 * {{{val a1, a2 = expression}}}
 *
 * It is like having:
 * {{{
 *   val a1 = expression
 *   val a2 = expression
 * }}}
 *
 * If expression is a side-effecting function, then a1 and a2 will be different
 * that is what is happening with the Enumeration trick
 *
 */
val sRng = new Random()
val a1, a2 = sRng.nextInt()

/**
 * A Function that returns the enumeration internally stored as a set.
 * see [[scala.Enumeration#values()]]
 */
Weekday0.values

/**
 * see [[scala.Enumeration#values()]]
 */
//Weekday0.values _

//Return the associated value Object.
//Note that introspection is used to picked up that name
//when constructing the Enum
Weekday0.withName("Monday")

//Return the serialization String of the Name
Weekday0.Friday.toString

//Order comes from the declaration when not specified (see below)
Weekday0.values.toList



//Set up the name directly
object Weekday2 extends Enumeration {
  val Monday = Value("Mo.")
  val Tuesday = Value("Tu.")
  val Wednesday = Value("We.")
  val Thursday = Value("Th.")
  val Friday = Value("Fr.")
  val Saturday = Value("Sa.")
  val Sunday = Value("Su.")
}

//Return the assigned name of Monday
Weekday2.Monday.toString
//To avoid the toString Method that does not show Monday
Weekday2.withName("Mo.") == Weekday2.Monday


//Sorting them the way we want
object Weekday4 extends Enumeration {
  val Monday = Value(1)
  val Tuesday = Value(2)
  val Wednesday = Value(3)
  val Thursday = Value(4)
  val Friday = Value(5)
  val Saturday = Value(6)
  val Sunday = Value(0)
}
Weekday4.values
Weekday4.values.toList

/**
 * == The issue 1 = Type Erasure ==
 *
 *
 * {{{object Weekday extends Enumeration {
    val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
 }

 object OtherEnum extends Enumeration {
   val A, B, C = Value
 }

 def test(enum: Weekday.Value) = {
   println(s"enum: $enum")
 }

 def test(enum: OtherEnum.Value) = {
   println(s"enum: $enum")
 }

 <console>:25: error: double definition:
def test(enum: Weekday.Value): Unit at line 21 and
def test(enum: OtherEnum.Value): Unit at line 25
have same type after erasure: (enum: Enumeration#Value)Unit
         def test(enum: OtherEnum.Value) = {}}}
 */
val issue1: Unit = println("Type Erasure")

/**
 * Issue 2
 */

def nonExhaustive(weekday: Weekday0.Value): Unit = {
  import Weekday0._
  weekday match {
    case Monday => ()
    case Tuesday => ()
    case Wednesday => ()
  }
}

