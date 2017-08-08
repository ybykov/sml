signature SET = 
sig
  type Elem
  type Set
  val empty: Set
  val insert: Elem * Set -> Set
  val member: Elem * Set -> bool
end

signature ORDERED =
sig
  type T
  val eq: T * T -> bool
  val lt: T * T -> bool
  val leq: T * T -> bool
end

structure IntOrdered: ORDERED =
struct
  type T = int
  fun eq(x, y) = x = y
  fun lt(x,y) = x < y
  fun leq(x,y) = x <= y
end

functor UnbalancedSet(Element: ORDERED): SET = 
struct
  exception ElementExists
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member2(x, E) = false
    | member2(x, T(a,y,b)) =
        if Element.lt(x,y) then member2(x,a)
        else if Element.lt(y,x) then member2(x,b)
             else true

  (* Exercise 2.2 *)
  fun member(x, E) = false 
    | member(x, T(E,y,E)) = Element.eq(x, y)
    | member(x, T(a,y,b)) =
        if Element.leq(x,y) then 
           member(x,a) orelse Element.eq(x,y)
        else member(x,b)

  fun insert2(x,E) = T(E,x,E)
    | insert2(x, s as T(a,y,b)) = 
        if Element.lt(x,y) then T((insert2(x,a),y,b))
        else if Element.lt(y,x) then T(a,y,insert2(x,b))
             else s

  (* Exercise 2.3 *)
  fun insert3(x, t) = 
    let fun checked_insert(x,E) = T(E,x,E)
          | checked_insert(x, s as T(a,y,b)) = 
                if Element.lt(x,y) then T((checked_insert(x,a),y,b))
                else if Element.lt(y,x) then T(a,y,checked_insert(x,b))
                else raise ElementExists
    in
      checked_insert(x, t)
    end
      handle ElementExists => t

  (* Exercise 2.4 *)
  fun insert(x, t) = 
    let fun checked_insert(x,E) = T(E,x,E)
          | checked_insert(x, s as T(a,y,b)) = 
                if Element.lt(x,y) then T((checked_insert(x,a),y,b))
                else if Element.lt(y,x) then T(a,y,checked_insert(x,b))
                else raise ElementExists
    in
      checked_insert(x, t)
    end
      handle ElementExists => t

end

structure IntSet = UnbalancedSet(IntOrdered)
