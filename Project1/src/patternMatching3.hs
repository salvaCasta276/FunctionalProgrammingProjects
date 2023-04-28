import Data.List
import Data.Maybe
import Utilities
import System.Environment (getArgs)
import System.Random
import Data.Char

main :: IO ()
main = do
  args <- getArgs
  --let arg1 = (head . head) (args)
  let arg3 = args !! 1
  --let arg4 = args !! 2
  --let arg5 = args !! 3
  --let arg6 = args !! 4
  --let arg7 = args !! 5
  let result = reduce (words "can you please tell me what Haskell is")
  --let result = rulesCompile [ ("", ["Speak up! I can't hear you."]), ("I need *", ["Why do you need * ?", "Would it really help you to get * ?", "Are you sure you need * ?"]), ("Why don't you *", ["Do you really think I don't * ?", "Perhaps eventually I will * .", "Do you really want me to * ?"]), ("Why can't I *", ["Do you think you should be able to * ?", "If you could * , what would you do?"])]
  --let result = reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
  --let result = match arg1 arg4 arg3
  --let result = transformationsApply arg1 id arg3 [(arg4, arg5), (arg6, arg7)]
  print result


-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a t s = concat (replace [a] (map pure t) s)
      
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
    | p == wc = if isPrefixOf (takeWhile (/=wc) ps) xs then (mmap (x:) (match wc ps xs)) >>= (Just . pure . head) else (mmap (x:) (match wc (wc:ps) xs))
--    | p == wc = orElse (singleWildcardMatch wc (p:ps) (x:xs)) (longerWildcardMatch wc (p:ps) (x:xs))
    | p == x = match wc ps xs
    | otherwise = Nothing

-- Helper function to match
--singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
--singleWildcardMatch (wc:ps) (x:xs) = if isPrefixOf (takeWhile (/=wc) ps) xs then (x:match wc ps xs) >>= ((\x -> Just [x]) . head) else Nothing
--longerWildcardMatch (wc:ps) (x:xs) = if isPrefixOf (takeWhile (/=wc) ps) xs then Nothing else (x:match wc (wc:ps) xs)

transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f s pt = mmap (substitute wc (snd pt)) (mmap f (match wc (fst pt) s))

transformationsApply :: Eq a => a -> ([a] -> [a]) -> [a] -> [([a], [a])] -> Maybe [a]
transformationsApply wc f s pts = listToMaybe (mapMaybe (transformationApply wc f s) pts)

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]

reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem "*.,:;!#%&|") 

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reflect :: Phrase -> Phrase
reflect = map (try (flip lookup reflections))

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply pairs = try (flip (transformationsApply "*" reflect) pairs) 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (map2 (prepare, map prepare))

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply pairs = try (flip (transformationsApply "*" id ) pairs)