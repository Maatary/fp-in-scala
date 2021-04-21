import IO.unit


/**
 * We can formalize this insight a bit. Given an impure function f of type A => B, we can split f into two functions:
 *
 *   - A pure function of type A => D, where D is some description of the result of f.
 *   - An impure function of type D => B, which can be thought of as an interpreter of these descriptions.
 *
 *
 *  It turns out that even procedures like println are doing more than one thing.
 *  And they can be factored in much the same way, by introducing a new data type that we’ll call IO
 *
 *  -The meaning is that println indeed does a lot of things which end with the actual side effects, "printing to the console".
 *   See Internal of Println.
 *
 *  -"And they can be factored in much the same way" i.e. the pure part and the impure part.
 *
 *  This is weird but we can think of it as stating what needs to happen and actually manipulating the console.
 *  It is like putting together what needs to happen, and actually making it happen.
 *
 *  But the example and explanation is weird because then we keep println. We just create something that describe what needs to happens,
 *  and when run, execute the side effect. But if println was doing both things already, then we have done nothing but create a structure that reproduce / mimic
 *  what println does internally and putting some actual control into it.
 *
 *  So if internally, println put together a description of what needs to happen, before executing something that make it happen,
 *  With that structure, we implement that ability to put together what needs to happen and have some control on it and when things will happen ultimately.
 *
 *  Yes a description of what needs to happen is a value and not the effect.
 *
 *
 *  "All it has given us is the ability to delay when something happen"
 *
 *
 *
 *
 */

sealed trait IO[A] { self =>

  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    def run: B = f(self.run)
  }

  // Here we build a function see `def run`
  def flatMap[B](g: A => IO[B]): IO[B] = new IO[B] {
    def run = g(self.run).run
  }

  //That's not building a function
  def _flatMap[B](g: A => IO[B]): IO[B] = {
    g(self.run)
  }

  def ++(io: IO[A]): IO[A] = new IO[A] {
    def run: A = {self.run; io.run}
  }

}

object IO {

  def unit[A](a: => A): IO[A] = new IO[A] {def run = a}

  def apply[A](a: => A): IO[A] = unit(a)

  def sequence[A](l: List[IO[A]]): IO[List[A]] = {
    l.foldRight(unit(Nil:List[A])) {(a, b) => a.flatMap{e => b.map(e::_)} }
  }
  def replicateM[A](n:Int)(io: IO[A]) = {
    sequence[A](List.fill[IO[A]](n)(io))
  }

  def empty: IO[Unit] = new IO[Unit] { def run: Unit = () }

  def printLine(msg: String): IO[Unit] = new IO[Unit] {def run: Unit = println(msg)}

  def buildMsg(msg:String) = new IO[String] {def run = msg}

  def forever[A,B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
    a flatMap (_ => t)
  }
}

/**
 * PlayGround
 */

/*
val e0 = IO.printLine("Hello") ++ IO.printLine("IO Introduction")
e0.run

val e1 = IO.buildMsg("Hello") ++ IO.buildMsg("IO Introduction")
e1.run



//import scala.io.StdIn._
//readLine()

val e3 = IO.printLine("Hello").flatMap(_ => IO.printLine("IO Introduction"))
e3.run

val e4 = for {
  fst <- IO.printLine("Hello")
  snd <- IO.printLine("IO Introduction")
} yield ()

e4.run*/

import IO._
import cats.Monad

/*val prog = for {
  x <- IO[String] { "hello" }
  y <- IO[String] { "IO Introduction"}
  _ <- IO {println(x + " " + y)}
} yield ()

prog.run

replicateM(3)(IO{ "replicate 3 times"}).run

import cats.syntax.all._

Monad[Option].replicateA(3, Some(10))

val list0 = /*List[Either[Int, Int]](Right(10))*/ List.fill[Either[Int, Int]](3)(Right(10))

val list1 = /*List[Option[Int]](Some(1), Some(2))*/ List.fill[Option[Int]](3)(Some(1))

list0.sequence

list1.sequence

Some(2).flatMap(_ => Some(3))*/



/*def factorial(n: Int) = {
  def factorialRec(acc: Int, n: Int): Int =  n match {
    case 0 => acc
    case _ => factorialRec(acc * n, n - 1)
  }
  factorialRec(1, n)
}

val io1 = IO { println("Exc1") }.flatMap{_ => IO { println("Exec2") }}



io1.run

val io2 = IO { println("Exc10") }._flatMap{_ => IO { println("Exec22") }}



println("after")




io2.run*/

IO.forever(IO{"println"}).run