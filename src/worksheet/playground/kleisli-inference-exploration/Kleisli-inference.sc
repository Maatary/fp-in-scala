import cats.Id
import cats.data.Kleisli
import cats.syntax.all._
import cats.data.Kleisli._


val hasInt: Kleisli[Id, Int, Int] = Kleisli.ask[Id, Int]

val hasString: Kleisli[Id, String, String] = Kleisli.ask[Id, String]


/**
 * This can't be satified.
 * That is why Zio  has the HAS Type
 * see [[https://www.youtube.com/watch?app=desktop&v=1e0C0jUzup4&feature=share| Exploring ZIO's Has Type]]
 *
 * Has[Int] with Has[String]
 *
 * Where Has is like a map Type -> Value
 */
val hasInthasString: Kleisli[Id, String with Int, (String, Int)] = hasInt.flatMap(int => hasString.map(_ -> int))