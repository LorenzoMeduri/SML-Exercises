fun euclid (x:int,y:int) = 
    if y <> 0
    then euclid (y, x mod y)
    else x;