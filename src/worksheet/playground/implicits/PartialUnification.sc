

def show[F[_], A](f: F[A]) = println(f)

show(List(1, 2, 3))
show(Some("hello"))




val err: Either[Error, String] = Left(new Error("boom"))

// doesn't not compile in 2.12.x, but in 2.13.x Partial unification is by default !
show(err)

//How to make it work in scala 2.12.x
type ErrorEither[A] = Either[Error, A]
show(err: ErrorEither[String])


/**
 * This is a Type lambda that use Any. Not wise. We should properly fix the type
 *
 * A Proper Approach would be (see next section)
 *
 * - type R[A] = Either[Error, A] where Error is fixed
 * - type R[A] = ({type F[A] = Either[Error, A]})#F
 */
//type R[A] = Either[_, A] // basically, "Any"
type R[A] = ({ type F[E] = Either[E, A] })#F[_] // type lambda and projection
//type R[A] = Either[E, A] forSome { type E }     // existential types

show(Right(5): R[Int])
show(Right("hello"): R[String])
show(Left(5): R[String])

/**
 * Typical way to write it
 *
 * Note that when using type alias we have #F[A] where with inline type lambda we would only have #F
 *
 * That's because on one side we are defining a type function, and the other an anonymous type function (i.e. type lambda).
 * Put it differently, the type lambda alone define the type constructor which is there used in the type alias.
 * Type lambda is an anonymous type function
 *
 */
type S[A] = ({type F[E] = Either[Error, E]})#F[A]

//val e1: S[Int] = Right("hello") fail of course




import scala.reflect.runtime.universe._

def ashow[F[_], A: TypeTag](f: F[A])(implicit tt: TypeTag[F[A]]) = {
  println(s"${typeOf[F[A]]} and A: ${typeOf[A]}")
}


val aOption: Option[Int] = Some(1)
ashow(aOption)

val afunc: Int => String = _.toString
ashow(afunc)

ashow(5)