import Data.List
import Data.Maybe
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let arg1 = (head . head) (args)
  let arg2 = args !! 1
  let arg3 = args !! 2
  print arg1
  print arg2
  print arg3
  let result = match arg1 arg2 arg3 
  print result

substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a t s = concat (substituteByParts [a] (map (\x -> [x]) t) (replicate (countElem a t) s))

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wildcard p s = (>>=) (listToMaybe (filter (\x -> concat (substituteByParts [wildcard] (map (\x -> [x]) p) x) == s) (productSpace (replicate (countElem wildcard p) (contSubsequences s))))) listToMaybe

-- Returns a list with all of the contiguous subsequences of xs, not including the empty list
-- It may contain repeated elements
contSubsequences :: [a] -> [[a]]
contSubsequences xs = filter (not . null) (concat (map inits (tails xs)))

-- Substitues into the n appearances of a in ys the first n elements of xs, assuming xs has at least n elements
substituteByParts :: Eq a => a -> [a] -> [a] -> [a]
substituteByParts _ [] _ = []
substituteByParts _ ys [] = ys
substituteByParts a (y:ys) (x:xs) 
    | y /= a = y:substituteByParts a ys (x:xs)
    | otherwise = x:substituteByParts a ys xs

-- Counts the number of appearances of a in the list as
countElem :: Eq a => a -> [a] -> Int
countElem a as = length (filter (\x -> x == a) as)

-- Given a list xs and a nonnegative integer n, it generates the set xs^n
-- Observe that it may contain repeated elements, depending if xs has repeated elements or not
productSpace :: [[a]] -> [[a]]
productSpace [] = [[]]
productSpace (x:xs) = [a:as | a <- x, as <- productSpace xs] 

