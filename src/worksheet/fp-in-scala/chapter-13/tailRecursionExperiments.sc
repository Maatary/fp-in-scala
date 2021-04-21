

//-g:notailcalls

def forever(call: Unit): Unit =
  forever (call)


forever(println("hello world"))