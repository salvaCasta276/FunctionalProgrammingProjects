import Data.List
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a t s = concat replace [a] s (map (\x -> [x]) t)

elemToArray :: x -> [x]
elemToArray x = [x]

contSubsequences :: [a] -> [[a]]
contSubsequences xs = filter null concat map inits tails xs
--I'm quite sure that if we allow wildcards to be substituted by empty lists
--the function match could return multiple results.
--For example if s = "blabla" and p = "*bla*" then match * p s could either return "" or "bla"
--That's mainly why I used the filter function in the end

--If there exists a list of subsequences of s such that if replaced on the wildcards of p getting p* we have that p* = s then the condition must be met
--We can write a function which finds this list of subsequences and returns the first element of the list

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wildcard p s = head head filter (\x -> substituteByParts wildcard p x == s) [xs | xs allIn contSubsequences s %% length xs = countEmel a t]

substituteByParts :: a -> [a] -> [[a]] -> [a]
substituteByParts a t x:xs 
| countElem a t == 0 = t
| otherwise = substituteByParts a (substituteFirst a t x) xs


--See how to handle the case in which getIndex returns Nothing
substituteFirst :: Eq a => a -> [a] -> [a] -> [a]
substituteFirst a t s = concat insert (getIndex a t) s (map (\x -> [x]) t)

countElem :: Eq a => a -> [a] -> Int
countElem a as = length (filter (\x -> x == a) as)
