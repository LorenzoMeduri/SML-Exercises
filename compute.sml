(*
 Consider the following data type, which represents a simple expression with two arguments x and y:

datatype Expr = X
              | Y
              | Avg of Expr * Expr
              | Mul of Expr * Expr

The constructor X represents the value of the first argument x of the expression.
The constructor Y represents the value of the second argument y.
The constructor Avg, which is applied to a pair (e1, e2), represents the (integer) average of the values of e1 and e2.
The constructor Mul, also applied to a pair (e1, e2), represents the product of the values of two expressions e1 and e2.
Implement a function called compute (with type Expr -> int -> int -> int) that computes the value of the expression
received as the first argument, using the values provided as the second and third arguments. 
It returns an integer indicating the final result of the evaluation.
*)

datatype Expr = X
              | Y
              | Avg of Expr * Expr
              | Mul of Expr * Expr;

fun compute (exp:Expr) =
    fn x =>
        fn y =>
            case exp of 
                X => x |
                Y => y |
                Avg (e1,e2) => ((compute e1 x y) + (compute e2 x y)) div 2 |
                Mul (e1,e2) => ((compute e1 x y) * (compute e2 x y));