(*
Write a function called treeDepth (tree -> int) 
that returns the depth of the tree (the longest path from the root to a leaf)
*)

datatype tree = Empty | Node of int*tree*tree | Leaf of int;

fun treeDepth (Empty) = 0 | treeDepth(Leaf t) = 1 | 
            treeDepth(Node (e,left,right)) = 
                let
                    val rightDepth = treeDepth(right)
                    val leftDepth  = treeDepth(left)
                in
                    if rightDepth > leftDepth
                    then 1+rightDepth
                    else 1+leftDepth
                end;
