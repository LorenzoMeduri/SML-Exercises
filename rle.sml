(*
Write a function RLE (string -> string) that implements the Run-Length Encoding algorithm.
*)
fun nFirst ("") = 0 | nFirst (s:string) = 
    let
        val l = explode s
    in
        if tl(l) = nil
        then 1
        else if(hd(l) = hd(tl(l))) then 1+nFirst(implode(tl l)) else 1
    end;

fun remN ("",n) = "" | remN (s:string,0) = s |remN (s:string,n) = 
    let
        val l = explode s
    in
        if tl(l) = nil 
        then ""
        else remN(implode(tl(l)),n-1)
    end;


fun RLE ("") = "" | RLE (s:string) =
    let
        val l = explode s
    in
        Int.toString(nFirst(s))^String.str(hd(l))^RLE(remN(s,nFirst(s)))
    end;