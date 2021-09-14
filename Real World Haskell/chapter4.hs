import System.Environment (getArgs)
import Data.Char

-- part 1
-- 2. Write a function splitWith that acts similarly to words but takes a predicate and a list of any type, and then splits its input list on every element for which the predicate returns False:

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p list =
    let (pre, suf) = (break p list)
    in pre : (cut suf)
    where cut [] = []
          cut [x] = []
          cut (x:xs) = (splitWith p xs)

-- 3. Using the command framework from the earlier section “A Simple Command-Line Framework” on page 71, write a program that prints the first word of each line of its input.

firstWord :: String -> String
firstWord s = unlines (map (\x -> (fst (break (\c -> c == ' ') x))) (splitLines s))

splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

interactWith function inputFile outputFile = do input <- readFile inputFile
                                                writeFile outputFile (function input)
main = mainWith myFunction
       where mainWith function = do
               args <- getArgs
               case args of
                   [input,output] -> interactWith function input output
                   _ -> putStrLn "error: exactly two arguments needed"
             myFunction = firstWord

-- 4. Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

transposes :: [String] -> String
transposes [] = ""
transposes (x:y:xs) = if (length xs) < 2
                      then (unlines (zipWith (\c d -> (c:d:[])) x y))
                      else (unlines (zipWith (\c d -> (c:d:[])) x y)) ++ (transposes xs)
-- part 2
-- 1. Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function from the earlier section “Explicit Recursion” on page 85.
-- 2. Your function should behave as follows
-- 3. Extend your function to handle the following kinds of exceptional conditions by calling error

asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold ('-':xs) = -1 * (foldl (\c d -> c * 10 + (digitToInt d)) 0 xs)
asInt_fold list = (foldl (\c d -> c * 10 + (digitToInt d)) 0 list)

-- 4. The asInt_fold function uses error, so its callers cannot handle errors. Rewrite the function to fix this problem:

-- type ErrorMessage = String

-- asInt_either :: String -> Ei

-- 5. The Prelude function concat concatenates a list of lists into a single list and has the following type:
-- concat :: [[a]] -> [a]
-- 6. Write your own definition of concat using foldr.

myConcat :: [[a]] -> [a]
myConcat = foldr  (++) []

-- 7. Write your own definition of the standard takeWhile function, first using explicit recursion, and then foldr.

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs) = if (p x)
                     then x : (myTakeWhile p xs)
                     else []

takeWhileFolder :: (a -> Bool) -> [a] -> [a]
takeWhileFolder _ [] = []
takeWhileFolder p list = foldr pick [] list
                         where pick x y = if (p x)
                                          then x:y
                                          else []

-- 8. The Data.List module defines a function, groupBy, which has the following type:
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- 9. Use ghci to load the Data.List module and figure out what groupBy does, then write your own implementation using a fold.

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy _ [] = [[]]
myGroupBy p list = (foldr eat [[]] list)
               where eat x [[]] = [[x]]
                     eat x y@(z:zs)
                       | (p x (head z)) = (x:z) : zs
                       | otherwise = [x]:y

-- 10. How many of the following Prelude functions can you rewrite using list folds?
-- • any

myAny :: (a -> Bool) -> [a] -> [a]
myAny p = (foldr eat [])
          where eat x y = if (p x)
                          then (x:y)
                          else y
-- • cycle

myCycle :: [a] -> [a]
myCycle list = foldr (++) [] (repeat list)

-- • words

myWords [] = [[]]
myWords list = (foldr eat [[]] list)
               where eat x y@(z:zs)
                       | isSpace x = [] : y
                       | otherwise = (x:z):zs

-- • unlines

myUnlines :: [String] -> String
myUnlines = (foldr eat "")
            where eat x y = x++"\n"++y
