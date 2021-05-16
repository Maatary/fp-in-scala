

sealed trait Animal
case class Dog() extends Animal
case class Cat() extends Animal


def matchListAnimal2(l: List[Animal]): Unit = l match {
  case l:List[Dog] => ()
}