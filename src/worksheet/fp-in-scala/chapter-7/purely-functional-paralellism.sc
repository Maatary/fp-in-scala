



/**
 *  = Key Points =
 *
 *
 *  --  Separating the concern of describing a computation from actually running it.
 *
 *
 *  --  We expand from modelling the Results (i.e. Effect) of a Computations to modelling the Computation themselves.
 *
 *
 *  --  Note that in this content IO computation introduce in chapter 13 are just one an example of it.
 *
 *
 *  --  Where we only had the Effect (Result) of a Computation was a Value, now the Computation itself is a value.
 *
 *
 *  --  This is all for referential transparency
 *
 *
 *  --  In this chapter emphasis will be put on algebraic reasoning and the idea that an API can be described by an algebra that obeys specific laws.
 *
 *
 *  === Refresher ===
 *
 *  - `An expression e is referentially transparent if for all programs p all occurrences of e in p can be replaced by the result of evaluating e without affecting the meaning of p.`
 *
 *  - `A function f is pure if the expression f(x) is referentially transparent for all referentially transparent x`.
 *
 *
 *
 */


case class test()


