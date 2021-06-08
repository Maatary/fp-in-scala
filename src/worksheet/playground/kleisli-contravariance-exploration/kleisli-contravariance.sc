/**
 *  == Principle of the highest Lower Bound ==
 *
 *  In the code bellow, the parameter type A must be assigned the types A1 and A2 at the same time.
 *  Together with the contravariance requirement, the compiler infer the Type that enables it,
 *  and that is the Highest Lower Bound: A1 with A2.
 *
 *  It allows the contravariance constraint to be respected.
 *
 *  A Parameter `Function1 [A1 with A2, R]` can be assigned a value `Function1 [A1, R]` or `Function1 [A2, R]`
 *
 *  This is Contravariance.
 *
 *
 *  == From Github issue: On the signature of flatMap in cats ==
 *
 * {{{def flatMap[C, AA <: A1](f: B => Kleisli[F, AA, C])(implicit F: FlatMap[F]): Kleisli[F, AA, C] }}}
 *
 *
 *  - `A1` is `upper bound` of `A2 with A1`,  so `A2 with A1` is `Highest Lower Bound` of `A1 or A2`, since `A2 with A1 <: A1 also A2 with A1 <: A2`
 *
 *  - `+` means subtype can be in supertype's place by covariance
 *
 *  - now `A` is `-A` so `supertype Kleisli[F, A1, B]` is safe to be in its `subtype Kleisli[F, A2 with A1,B] place`
 *
 *  - it should be pretty easy for the compiler to infer that a `subtype for both A1 and A2` must be `A2 with A1`
 *
 *  - by the constrain `AA <: A1`, AA must be a subtype of A1
 *
 *  - if you pass in f as {{{f: B => Kleisli[F, A2, C]}}} since contravariant,
 *  super type can be in subtype's position,
 *  f's type can be variant to any of A2 subtype,
 *  so AA must be subtype of both A2 and A1
 *
 *
 */


case class Kleisli[F[_], -A, B](run: A => F[B])

def first[A, B, C, F[_]](a: Kleisli[F, A, B], b: Kleisli[F, A, C]) = a

trait A1
trait A2
//trait A12 extends A1 with A2

first(Kleisli((a: A1) => Some(a)), Kleisli((b:A2)=>Some(b)))