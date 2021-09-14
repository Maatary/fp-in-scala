import java.util.regex._
import scala.util.matching.Regex

type ParseError

trait Parsers[Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]


  def char_(c: Char): Parser[Char]

  def string(s:  String): Parser[String]


  /**
   *  We expect that or(string("abra"),string("cadabra")) will succeed whenever either string parser succeeds
   */
  def or[A](pa: Parser[A],  pb: Parser[A]): Parser[A]

  /**
   * '''map''' Combinator . It simply transform the result of the parsing.
   */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]





  def char(c: Char): Parser[Char] = map(string(c.toString)) (_.charAt(0))

  def numA(s:String): Parser[Int] = map(string(s))(_.size)

  /**
   * This is like Unit in other monad.
   * It is  a parser which  no matter what always succeed in retuning an A when ran
   * Because String("") will always succeed for any string, and we map over it,
   * ignoring its result that we transform into a.
   */
  def succeed[A](a: A) = map(string("")) (_ => a)

  /**
   * We call this combinator slice since we intend for it to return the portion of the input string examined by the parser if successful.
   * As an example, run(slice(('a'|'b') .many))("aaba") results in Right("aaba")
   * We ignore the list accumulated by many and simply return the portion of the input string matched by the parser.
   */
  def slice[A](p: Parser[A]): Parser[String]


  /**
   * Product is like tupled in cats, hence it imply a flatmap
   *
   * `product` is associative. These two expressions are "roughly" equal:
   *
   * {{{(a ** b) ** c}}}
   * {{{ a ** (b ** c)}}}
   *
   * The only difference is how the pairs are nested. The `(a ** b) ** c` parser returns an `((A,B), C)`, whereas the `a ** (b ** c)` returns an `(A, (B,C))`. We can define functions `unbiasL` and `unbiasR` to convert these nested tuples to flat 3-tuples:
   *
   * {{{def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)}}}
   *
   * {{{def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)}}}
   *
   * With these, we can now state the associativity property:
   *
   * {{{(a ** b) ** c map (unbiasL) == a ** (b ** c) map (unbiasR)}}}
   *
   * We'll sometimes just use `~=` when there is an obvious bijection between the two sides:
   *
   * {{{(a ** b) ** c ~= a ** (b ** c)}}}
   *
   * `map` and `product` also have an interesting relationship--we can `map` either before or after taking the product of two parsers, without affecting the behavior:
   *
   * {{{a.map(f) ** b.map(g) == (a ** b) map { case (a,b) => (f(a), g(b)) }}}}
   *
   * For instance, if `a` and `b` were both `Parser[String]`, and `f` and `g` both computed the length of a string, it doesn't matter if we map over the result of `a` to compute its length, or whether we do that _after_ the product.
   *
   *
   *  === Note ===
   *
   *  Non strict here so if the first fail, you don't do the second.
   *
   *
   */
  def product[A,B](pa: Parser[A], pb: => Parser[B]): Parser[(A,B)]


  def map2[A, B, C](pa:  Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] = {
    map(product(pa, pb)) (f.tupled)
  }

  /**
   * '''many''' combinator definition. It recognize 0 or more character
   * and return the result as a List.
   *
   * Now that we have map2, is many really primitive? Let’s think about what many(p) will do. It tries running p, followed by many(p) again, and
   * and so on until the attempt to parse p fails. It’ll accumulate the results of all successful runs of p into a list.
   * As soon as p fails, the parser returns the empty List.
   *
   * === One thing to notice ===
   *
   *  -- 0 or more, is  if you fail the first time, you return the empty list
   *  -- 0 is the failure to parse P.
   *  -- If you succeed you keep going.
   *
   *  === Notable difference with case  where we  have  this that parttern i.e. operating over a list i.e. a recursive structure ===
   *
   *  -- Here the assumption for now is that we  will operate over a string which is a list of char after all.
   *
   *  -- But i think that generally speaking we are abstractive over the recursive structure we are operating on.
   *
   *  -- Or more generally, we are abstracting over the thing we are operating on over  which  we are going recursively.
   *
   *  -- Parser type at this point is not even defined yet. We just know it is a computation we are composing with/over.
   *
   *  -- The trick confusing differenc here is that typical, we recurse over a a list that we pattern match  (deconstruct ourself) and therefore pass the argument explicitly.
   *
   *  -- Here we are not doing that, and it is new and feels strange.
   *
   *  -- Some resemblance yet difference is that, where usually we would define the terminal case as end of the list i.e. nil, here the terminal case is failure to parse.
   *
   *  -- Are we able to do that because we are abstract, we are modeling a computation ?
   *
   *  -- Because in fact nothing would stop us to use that to parse a list and do whatever ?
   *
   */
  def many[A](p: Parser[A]): Parser[List[A]] = {
    or(map2(p, many(p))(_ :: _), succeed(List()))
  }


  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p)) (_ :: _)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List[A]())
    case _ => map2(p, listOfN(n -1, p)) (_::_)
  }



}