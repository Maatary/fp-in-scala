/**
 * Experiment conducted with -g:notailcalls
 *
 * -- It confirms that calls are stacked even when the recursive call is in tail position.
 *
 * -- Otherwise it is a while loop and the program never stops.
 *
 * -- This is useful to understand algorithm like trampolining.
 *
 */


def forever(call: Unit): Unit =
  forever (call)


forever(println("hello world"))