(*
Write a function isIncreasing that given an int list returns true if it is ordered in ascending order and false otherwise 
*)

fun isIncreasing [] = true | isIncreasing [e] = true | isIncreasing ((e1::e2::l):int list) = 
        if e2>e1 then isIncreasing(e2::l) else false;