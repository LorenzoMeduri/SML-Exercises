(*
Write a function called treeDepth (tree -> int) 
that removes all nodes and leaves below a given depth d, where the root is at depth 0.
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
