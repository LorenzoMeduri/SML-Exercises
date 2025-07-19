(*Write a function noduplen (with type ''a list -> int) that gets as argument an ''a list 
and returns the number of elements without counting duplicates*)

fun noduplen [] = 0 | noduplen ((e::l):''a list) = 
    if (List.exists (fn x => x=e) l)
    then 0+noduplen(l)
    else 1+noduplen(l);