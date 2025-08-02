(*
Write a function called pruneTree (int -> tree -> tree) 
that removes all nodes and leaves below a given depth d, where the root is at depth 0.
*)

datatype tree = Empty | Node of int*tree*tree | Leaf of int;

fun pruneTree (d:int) = 
    if d = 0 
    then 
        fn Empty => Empty | l as Leaf _ => l | Node(e,left,right) => Node(e,Empty,Empty)
    else 
        fn Empty => Empty |
            l as Leaf _ => l |
            n as Node (e,left,right) => Node(e,pruneTree (d-1) left,pruneTree (d-1) right);