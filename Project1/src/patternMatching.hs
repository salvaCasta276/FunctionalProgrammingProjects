import Data.List

substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a t s = concat (replace [a] (map (\x -> [x]) t) s)

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wildcard p s = (head . head) (filter (\x -> substituteByParts [wildcard] (map (\x -> [x]) p) x == s) (productSpace contSubsequences (countElem wildcard p)))

-- Returns a list with all of the contiguous subsequences of xs, not including the empty list
-- It may contain repeated elements
contSubsequences :: [a] -> [[a]]
contSubsequences xs = filter (not . null) (concat (map inits (tails xs)))

-- Substitues into the n appearances of a in ys the first n elements of xs, assuming xs has at least n elements
substituteByParts :: Eq a => a -> [a] -> [a] -> [a]
substituteByParts _ [] _ = []
substituteByParts _ ys [] = ys
substituteByParts a (y:ys) (x:xs) 
    | y != a = substituteByParts a ys (x:xs)
    | otherwise = x:substituteByParts a ys xs

-- Counts the number of appearances of a in the list as
countElem :: Eq a => a -> [a] -> Int
countElem a as = length (filter (\x -> x == a) as)

-- Replaces all the appearances of a in as for b
replace :: Eq a => a -> [a] -> a -> [a]
replace a as b = map (\x -> if x == a then b else a) as

-- Given a list xs and a nonnegative integer n, it generates the set xs^n
-- Observe that it may contain repeated elements, depending if xs has repeated elements or not
productSpace :: [a] -> Int -> [[a]]
productSpace _ 0 = [[]]
productSpace xs n = [a:as | a <- xs, as <- productSpace xs (n-1)] 
