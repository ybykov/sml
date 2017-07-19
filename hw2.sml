(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
*    string), then you avoid several of the functions in problem 1 having
*       polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
      s1 = s2

fun all_except_option(s, xs) = 
let fun helper (s,xs,acc) = 
  case xs of
       [] => NONE 
     | x::xs' => if same_string(s, x)
                 then SOME(acc @ xs')
                 else helper(s, xs', x::acc)
in
  helper(s, xs, [])
end


fun get_substitutions1(sl: string list list, s: string) =
  case sl of
       [] => []
     | x::xs'=> (case all_except_option(s, x) of
                     NONE => []
                   | SOME xs => xs) @ get_substitutions1(xs', s)

fun get_substitutions2(sl: string list list, s: string) =
let fun helper(sl: string list list, s: string, acc: string list) = 
  case sl of
       [] => acc 
     | x::xs'=> helper(xs', s, ((case all_except_option(s, x) of
                                     NONE => []
                                   | SOME xs => xs) @ acc))
in
  helper(sl, s, [])
end

fun similar_names(sl: string list list, {first: string, last: string, middle: string}) =
  let val alternative_names = get_substitutions2(sl, first) @ [first]
      fun construct(an: string list, acc) = 
        case an of
          [] => acc
        | x::xs => construct(xs, {first = x, last = last, middle = middle} :: acc)
  in
    construct(alternative_names, [])
  end

      (* put your solutions for problem 1 here *)

      (* you may assume that Num is always used with values 2, 3, ..., 10
      *    though it will not really come up *)
      datatype suit = Clubs | Diamonds | Hearts | Spades
      datatype rank = Jack | Queen | King | Ace | Num of int 
      type card = suit * rank

      datatype color = Red | Black
      datatype move = Discard of card | Draw 

      exception IllegalMove

      (* put your solutions for problem 2 here *)

