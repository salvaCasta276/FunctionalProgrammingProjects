import Data.List
import Data.Maybe
import Utilities
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


-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a t s = concat (replace [a] (map (\x -> [x]) t) s)
      
replace :: Eq a => a -> [a] -> a -> [a]
replace _ [] _ = []
replace a (x:xs) b
    | a == x = (b:replace a xs b)
    | otherwise = (x:replace a xs b)

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] (x:xs) = Nothing
match _ (p:ps) [] = Nothing
match wc (p:ps) (x:xs)
    | p == wc = if isPrefixOf (takeWhile (/=wc) ps) xs then (mmap (x:) (match wc ps xs)) >>= ((\x -> Just [x]) . head) else (mmap (x:) (match wc (wc:ps) xs))
--    | p == wc = orElse (singleWildcardMatch wc (p:ps) (x:xs)) (longerWildcardMatch wc (p:ps) (x:xs))
    | p == x = match wc ps xs
    | otherwise = Nothing

-- Helper function to match
--singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
--singleWildcardMatch (wc:ps) (x:xs) = if isPrefixOf (takeWhile (/=wc) ps) xs then (x:match wc ps xs) >>= ((\x -> Just [x]) . head) else Nothing
--longerWildcardMatch (wc:ps) (x:xs) = if isPrefixOf (takeWhile (/=wc) ps) xs then Nothing else (x:match wc (wc:ps) xs)

