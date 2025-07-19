(* Write a function called hist (with type real list => real*real => int) that takes as argument 
a list of reals and a tuple of reals (x,y). The function returns the number of elements of the 
list within the interval (x-y,x+y), extremes excluded *)

fun hist nil = (fn _ => 0) | hist ((e::l):real list) = 
    fn (x:real,y:real) => 
        if e>(x-y) andalso e<(x+y)
        then 1+hist l (x,y)
        else 0+hist l (x,y)