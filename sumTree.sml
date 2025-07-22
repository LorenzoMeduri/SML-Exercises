(*
Given this datatype:
datatype tree = Empty | Node of int*tree*tree | Leaf of int
Write a function named sumTree that takes a value of type tree and returns the sum of all integers stored in the tree.
*)

datatype tree = Empty | Node of int*tree*tree | Leaf of int

fun sumTree Empty = 0 | sumTree (Leaf n) = n | 
    sumTree (Node (n,left,right)) = n+sumTree(left)+sumTree(right);