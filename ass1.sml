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
