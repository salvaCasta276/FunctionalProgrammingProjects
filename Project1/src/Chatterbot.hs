module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.List
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
--stateOfMind b = return id
stateOfMind brain = do
  rand <- randomIO :: IO Int
  let randomBrain = map (\x -> (fst x, (snd x)!!(mod rand (length (snd x))))) brain
  return (\x -> rulesApply randomBrain x)
  --index <- floor $ (length brain) * r
  --return rulesApply brain!!(index)

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply pairs = try (transformationsApply "*" reflect pairs) 

reflect :: Phrase -> Phrase
reflect = map (try (flip lookup reflections))

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


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (map2 (prepare, map prepare))


--------------------------------------


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

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply pairs = try (transformationsApply "*" id pairs)


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

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
    | p == wc = if (not . null) nextWord && isPrefixOf nextWord xs || null nextWord && null xs
                  then (mmap (x:) (match wc ps xs)) >>= (Just . pure . head)
                  else (mmap (x:) (match wc (wc:ps) xs))
--    | p == wc = orElse (singleWildcardMatch wc (p:ps) (x:xs)) (longerWildcardMatch wc (p:ps) (x:xs))
    | p == x = match wc ps xs
    | otherwise = Nothing
    where nextWord = (takeWhile (/=wc) ps)

-- Helper function to match
--singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
--singleWildcardMatch (wc:ps) (x:xs) = if isPrefixOf (takeWhile (/=wc) ps) xs then (x:match wc ps xs) >>= ((\x -> Just [x]) . head) else Nothing
--longerWildcardMatch (wc:ps) (x:xs) = if isPrefixOf (takeWhile (/=wc) ps) xs then Nothing else (x:match wc (wc:ps) xs)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f s pt = mmap (substitute wc (snd pt)) (mmap f (match wc (fst pt) s))

transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f pts s = listToMaybe (mapMaybe (transformationApply wc f s) pts)

