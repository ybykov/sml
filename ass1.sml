fun is_older(d1: int*int*int, d2: int*int*int): bool =
  if #1 d1 <> #1 d2 then #1 d1 < #1 d2 else
    if #2 d1 <> #2 d2 then #2 d1 < #2 d2 else
      #3 d1 < #3 d2

fun number_in_month(l: (int*int*int) list, m: int): int =
  if null l
  then 0
  else (if #2 (hd l) = m then 1 else 0) + number_in_month(tl l, m)

fun number_in_months(l: (int*int*int) list, m: int list): int =
  if null m
  then 0
  else number_in_month(l, hd m) + number_in_months(l, tl m)
  
fun dates_in_month(l: (int*int*int) list, m: int): (int*int*int) list =
  if null l
  then []
  else 
    if #2 (hd l) = m 
    then (hd l) :: dates_in_month(tl l, m) 
    else dates_in_month(tl l, m)

fun dates_in_months(l: (int*int*int) list, m: int list): (int*int*int) list =
  if null m
  then []
  else dates_in_month(l, hd m) @ dates_in_months(l, tl m)

fun get_nth(xs: string list, n: int) = 
  if n = 1 
  then hd xs 
  else get_nth(tl xs, n-1)

fun date_to_string(d: int*int*int): string = 
let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
in
  get_nth(months, #2 d) ^ " " ^ Int.toString (#3 d) ^ ", " ^ Int.toString (#1 d)
end

fun number_before_reaching_sum(sum: int, l: int list): int =
let fun helper(sum: int, l: int list, acc: int, cur: int): int = 
        if sum <= acc
        then cur
        else helper(sum, tl l, acc + hd l, cur + 1)
in
  helper(sum, l, 0, ~1)
end

fun what_month(d: int) = 
let val m = [31,28,31,30,31,30,31,31,30,31,30,31]
in
  number_before_reaching_sum(d, m) + 1
end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(l: (int*int*int) list) = 
  if null l
  then NONE
  else
    let fun helper(d1: (int*int*int), d2: (int*int*int)) = if is_older(d1,d2)
                                                           then d1 else d2
        fun min(l: (int*int*int) list, acc: int*int*int) =
          if null l
          then acc
          else min(tl l, helper(acc, hd l))
    in
      SOME(min(l, hd l))
    end

fun number_in_months_challenge(xs: (int*int*int) list, ms: int list) = 
let fun exist(xs: int list, e: int): bool =
        if null xs
        then false
        else e = hd xs orelse exist(tl xs, e)
    fun unique(xs: int list): int list = 
      if null xs
      then []
      else if exist(tl xs, hd xs)
           then unique(tl xs)
           else hd xs :: unique(tl xs)
in
  number_in_months(xs, unique(ms))
end

fun dates_in_months_challenge(xs: (int*int*int) list, ms: int list) = 
let fun exist(xs: int list, e: int): bool =
        if null xs
        then false
        else e = hd xs orelse exist(tl xs, e)
    fun unique(xs: int list): int list = 
      if null xs
      then []
      else if exist(tl xs, hd xs)
           then unique(tl xs)
           else hd xs :: unique(tl xs)
in
  dates_in_months(xs, unique(ms))
end
