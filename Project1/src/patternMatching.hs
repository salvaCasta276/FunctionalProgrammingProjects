import Data.List
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a t s = concat (replace [a] (map (\x -> [x]) t) s)

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wildcard p s = head head filter (\x -> substituteByParts wildcard p x == s) [xs | xs allIn contSubsequences s && length xs = countEmel a t]

contSubsequences :: [a] -> [[a]]
contSubsequences xs = filter (not . null) (concat (map inits (tails xs)))

substituteByParts :: a -> [a] -> [[a]] -> [a]
substituteByParts a t x:xs 
| countElem a t == 0 = t
| otherwise = substituteByParts a (substituteFirst a t x) xs

--See how to handle the case in which getIndex returns Nothing
substituteFirst :: Eq a => a -> [a] -> [a] -> [a]
substituteFirst a t s = concat insert (getIndex a t) s (map (\x -> [x]) t)

countElem :: Eq a => a -> [a] -> Int
countElem a as = length (filter (\x -> x == a) as)

replace :: Eq a => a -> [a] -> a -> [a]
replace a as b = map (\x -> if x == a then b else a) as
