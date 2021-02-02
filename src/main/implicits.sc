

def sum[T](list: List[T])(implicit integral: Integral[T]): T = {

  //Don't like this: It just call the syntax which is part of the instance
  //Weird Way to build type class in scala Std Lib
  import integral._ // get the implicits in question into scope

  //list.foldLeft(integral.zero)(integral.plus(_, _))
  list.foldLeft(integral.zero)(_ + _)
}



