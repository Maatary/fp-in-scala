import cats.{Eval, Foldable}
import cats.syntax.all._



Foldable[List].foldRight(List(1,2,3,4),Eval.later(List[Int]()))((a, evalb) =>  evalb.flatMap(b => Eval.later(a::b))).value

