import cats.syntax.all._


val f = (s:String) => s.toUpperCase

val h = (s:String)  => s.toLowerCase


for {
  cap <- f
  low <- h
} yield cap -> low

f.flatMap { cap => h.map { low => cap -> low }  } ("MaaTari")

