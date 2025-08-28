(*
Given the datatype:
datatype Tree = Leaf of int | Node of int * Tree * Tree | Empty
which allows the representation of a binary tree of integers, write two functions:
- The first,  all_paths, of type (Tree -> int list list) takes a binary tree as input and returns a list of lists of integers,
where each list represents a path from the root to a leaf in the tree (the Empty node is not a leaf).
The paths must be ordered following a pre-order traversal, as should the nodes within each path.
- The second, filter_paths of type 'a list * ('a -> bool) -> 'a list (in the specific case int list list * (int list -> bool) -> int list list) takes as input:
a list of paths from root to leaf extracted from the tree, and a generic function of type (int list -> bool), which checks a condition on each path and returns true if the condition is met.
The function must return all the paths for which the condition (passed as the second argument) is satisfied.
In the case of an empty tree, the function should return the empty list ([]).
*)
(*
Examples

val tree = Node(1, Empty, Node(2, Node(3, Leaf 5,Node(6, Node(8, Empty, Leaf 10),Leaf 9)),Node(4, Empty, Node(7,Leaf 10 ,Empty))))


> all_paths(tree);
val it = [[1, 2, 3, 5], [1, 2, 3, 6, 8, 10], [1, 2, 3, 6, 9], [1, 2, 4, 7, 10]]: int list list

> fun sum_lower_than_30 list = (foldl (op +) 0 list) < 30;
val sum_lower_than_30 = fn: int list -> bool

> filter_paths(all_paths(tree), sum_lower_than_30);
val it = [[1, 2, 3, 5], [1, 2, 3, 6, 9], [1, 2, 4, 7, 10]]: int list list


*)

datatype Tree = Leaf of int | Node of int * Tree * Tree | Empty

fun all_paths Empty = [] | all_paths (Leaf l) = [[l]] | all_paths (Node(n,left,right)) =
    (map (fn l => (n::l)) (all_paths(left)))@(map (fn l => (n::l)) (all_paths(right)));

fun filter_paths (nil,f:'a -> bool) = nil | filter_paths ((e::l),f:'a -> bool) = 
    if f(e)
    then e::filter_paths(l,f)
    else filter_paths(l,f);