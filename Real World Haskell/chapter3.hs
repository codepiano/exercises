import Data.List

-- 1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function.
-- 2. Add a type signature for your function to your source file. To test it,load the source file into ghci again.

llen :: [a] -> Int
llen []     = 0
llen (x:xs) = 1 + (llen xs)

-- 3. Write a function that computes the mean of a list, i.e., the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating-point number.)

mean :: [Rational] -> Rational
mean [] = 0
mean x = (sum x) / (fromIntegral (length x))

-- 4. Turn a list into a palindrome; i.e., it should read the same both backward and forward. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].

palindrome :: [a] -> [a]
palindrome [] = []
palindrome list = list ++ (reverse list)
    where reverse [] = []
          reverse (x:xs) = (reverse xs) ++ [x]

-- 5. Write a function that determines whether its input list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome list | odd (length list) = False
isPalindrome list = list==(reverse list)
    where reverse [] = []
          reverse (x:xs) = (reverse xs) ++ [x]

-- 6. Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)

sortList :: [[a]] -> [[a]]
sortList [] = []
sortList list = sortBy (\a b -> compare (length a) (length b)) list

-- 7. Define a function that joins a list of lists together using a separator value:
--        file: ch03/Intersperse.hs intersperse :: a -> [[a]] -> [a]
-- 8. The separator should appear between elements of the list, but it should not follow the last element. Your function should behave as follows:

myIntersperse :: a -> [[a]] -> [a]
myIntersperse s [] = []
myIntersperse s [x] = x
myIntersperse s (x:xs) = x ++ [s] ++ (myIntersperse s xs)

-- 9. Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an Empty. For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

maxDepth :: (Tree a) -> Int
maxDepth Empty = 0
maxDepth (Node a x y) = max (depth 1 x) (depth 1 y)
    where depth n Empty = n
          depth n (Node _ x Empty) = depth (n + 1) x
          depth n (Node _ Empty y) = depth (n + 1) y
          depth n (Node _ x y) = max (depth 1 x) (depth 1 y)

-- 10. Consider three two-dimensional points, a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.

data Direction = MyLeft
               | MyRight
               | MyStraight
                 deriving (Eq, Show)

data Point = Point Double Double
             deriving (Eq, Show)

-- 11. Write a function that calculates the turn made by three two-dimensional points and returns a Direction.

calcDirection :: Point -> Point -> Point -> Direction
calcDirection (Point a b) (Point c d) (Point e f)
    | r > 0 = MyLeft
    | r < 0 = MyRight
    | r == 0 = MyStraight
    where v1 = c - a
          v2 = d - b
          u1 = e - c
          u2 = f - d
          r  = (u1 * v2) - (u2 * v1)

-- 12. Define a function that takes a list of two-dimensional points and computes the direction of each successive triple. Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction.

calcBatchDirection :: [Point]  -> [Direction]
calcBatchDirection (x:y:z:xs) = if length(xs) == 0
                                then [(calcDirection x y z)]
                                else (calcDirection x y z) : (calcBatchDirection (y:z:xs))

-- 13. Using the code from the preceding three exercises, implement Graham’s scan algorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, and how the Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia (http://en.wikipedia.org/).

-- See: https://rosettacode.org/wiki/Convex_hull#Haskell
