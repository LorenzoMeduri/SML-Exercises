(*
Write a function called sumEven (with type int -> int list -> int) that takes as argument an int n and an int list l.
The function adds to n all elements of l which are in an even position. If the list has <2 elements then the function returns n.
*)

fun sumEven (n:int) = 
    fn [] => n
    |  [e] => n
    |  (e1::e2::l) => e2+(sumEven n l);