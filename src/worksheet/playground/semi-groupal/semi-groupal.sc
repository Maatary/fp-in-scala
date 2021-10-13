import cats.syntax.all._


List(1,2,3,4).filterNot(List(1,2,3).contains(_))



val map1 = Map("https://data.elsevier.com/lifescience/schema/resnet/contains" -> List("https://data.elsevier.com/lifescience/schema/resnet/Pathway" -> "https://data.elsevier.com/lifescience/schema/resnet/Binding"))
val map2 = Map("https://data.elsevier.com/lifescience/schema/resnet/contains" -> List("https://data.elsevier.com/lifescience/schema/resnet/Pathway" -> "https://data.elsevier.com/lifescience/schema/resnet/Biomarker"))
val map3 = Map("https://data.elsevier.com/lifescience/schema/resnet/contains "-> List("https://data.elsevier.com/lifescience/schema/resnet/Pathway" -> "https://data.elsevier.com/lifescience/schema/resnet/CellExpression"))


map1 |+| map2


List(1,2,3) -- List(6,7)