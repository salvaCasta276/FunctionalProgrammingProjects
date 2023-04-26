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

-- Substitues into the n appearances of a in ys the first n elements of xs, assuming xs has at least n elements
substituteByParts :: Eq a => a -> [a] -> [a] -> [a]
substituteByParts _ [] _ = []
substituteByParts _ ys [] = ys
substituteByParts a (y:ys) (x:xs) 
    | y /= a = y:substituteByParts a ys (x:xs)
    | otherwise = x:substituteByParts a ys xs


-- Match will remove the string found before the first * and then use singleWM and longerWM to move one step
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wc ps xs = let
    psHead = takeWhile (/=wc) ps
    in if isPrefixOf psHead xs then longerWildcardMatch (removePrefix psHead xs) (removePrefix psHead ps) else Nothing


-- Helper function to match
singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = let
    psHead = takeWhile (/=wc) ps
    in if isPrefixOf psHead xs then (x:longerWildcardMatch (removePrefix psHead ps) (removePrefix psHead xs)) else longerWildcardMatch (wc:ps) (x:xs)

longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (wc:ps) (x:xs) = 

removePrefix :: Eq a => [a] -> [a] -> [a]
removePrefix [] t = Just t
removePrefix (s:ss) (t:ts)
    | s == t = removePrefix ss ts
    | otherwise = []



-------------------Way b------------------------

-- Given a char x and a list x it splits the list where the x char is found
split :: Eq a => [a] -> [[a]]
split x xs = map (delete x) (groupBy (\a b -> b /= x) xs)

f _ (t:ts) [] = -- There is no equality
f _ [] (s:ss) = -- There is no equality
f _ [] [] = []
f a (t:ts) ss
    | isPrefixOf t ss = f a ts (removePrefix t ss)
    | otherwise = (head ss:f a (t:ts) (tail ss))

-------------------Way a------------------------
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match a t s = getIndexes (parse a (a:t))

-- Generates a list containing all of the indexes where a appears as a sublist of s
getIndexes :: Eq a => [a] -> [a] -> [Int]
getIndexes a s = filter (/= -1) (map (\x -> fromJust (if isPrefixOf a x then elemIndex x (tails s) else Just (-1))) (tails s))

-- TODO See how to handle the case in which there is no valide index to take min or max over
getSolution :: Int -> [[Int]] -> [[a]] -> [Int]
getSolution n [x] [a] = [maximum (filter (>=n) x)]
getSolution n (x:xs) (a:as) = (minimum (filter (>=n) x):getSolution (minimum (filter (>=n) x) + length a) xs as)
