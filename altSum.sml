(*
Write a function called altSum that takes a list [a1,...,an] and computes a1-a2+a3-a4+...
The function must work with lists of any length
*)

fun altSum [] = 0 | altSum [e] = e | altSum (e1::e2::l) = 
    e1-e2+altSum(l);