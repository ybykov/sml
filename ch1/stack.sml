signature STACK = 
sig
  type 'a Stack
  val empty: 'a Stack
  val isEmpty: 'a Stack -> bool
  val cons: 'a * 'a Stack -> 'a Stack
  val head: 'a Stack -> 'a
  val tail: 'a Stack -> 'a Stack
  val concat: 'a Stack * 'a Stack -> 'a Stack
  val update: 'a Stack * int * 'a -> 'a Stack
end

structure List: STACK = 
struct
  exception EMPTY
  type 'a Stack = 'a list
  val empty = []
  fun isEmpty s = null s
  fun cons(x,s) = x :: s
  fun head s = hd s
  fun tail s = tl s
  fun concat([], ys) = ys
    | concat((x :: s), ys) = (x :: concat(s, ys))
  fun update([], i, v) = raise EMPTY
    | update((x :: s), 0, v) = v :: s
    | update((x :: s), i, v) = x :: update(s, i - 1, v)
end

structure CustomStack: STACK =
struct
  exception EMPTY
  datatype 'a Stack = NIL | CONS of 'a * 'a Stack
  val empty = NIL
  fun isEmpty NIL = true | isEmpty _ = false
  fun cons(x,s) = CONS(x,s)
  fun head NIL = raise EMPTY
    | head (CONS(x,s)) = x
  fun tail NIL = raise EMPTY
    | tail (CONS(x,s)) = s
  fun concat(NIL, ys) = ys
    | concat(CONS(x,s), ys) = cons(x, concat(s,ys)) 
  fun update(NIL, i, v) = raise EMPTY
    | update(CONS(x,s), 0, v) = cons(v, s)
    | update(CONS(x,s), i, v) = cons(x, update(s, i - 1, v))
end

fun suffixes [] = [[]]
  | suffixes (x::xs) = (x::xs) :: suffixes xs 
