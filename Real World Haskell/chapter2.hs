-- 2. Write a function, lastButOne, that returns the element before the last.

lastButOne (x:xs) = if length xs == 1
                    then x
                    else lastButOne xs
