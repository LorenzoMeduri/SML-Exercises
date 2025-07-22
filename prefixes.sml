(*Write a function called prefixes of type string => string list that takes as input a string and returns
the list of all its prefixes (the complete string included and the empty string excluded)
Example:
> prefixes "world";
val it : ["world" , "worl", "wor", "wo", "w"];
*)

fun prefixes "" = [] | prefixes (s:string) = 
    let fun removeLast (s:string) = substring(s,0,(size s)-1)
    in s::prefixes(removeLast(s))
    end;