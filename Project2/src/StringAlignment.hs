-- Salvador Castagnino, Aden McCusker
import Data.List

main :: IO ()
main = do
  let score = similarityScore "h-as" "-pas"
  putStrLn ("The score is: " ++ show score)
  --let maxLens = maximaBy length ["cs", "efd", "lth", "it"]
  --putStrLn ("The maxLens are: " ++ show maxLens)
  --outputOptAlignments "has" "pas"

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore xs [] = scoreSpace * length xs
  where
  scoreSpace = -10
similarityScore [] ys = scoreSpace * length ys
  where
  scoreSpace = -10
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + if x == y then scoreMatch else scoreMismatch,
                                        similarityScore xs (y:ys) + scoreSpace,
                                        similarityScore (x:xs) ys + scoreSpace]
  where
  scoreMatch = 1
  scoreMismatch = 0
  scoreSpace = -10

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter ((== maximum (map valueFcn xs)) . valueFcn) xs 

type AlignmentType = (String,String)

--This is wrong, -s shoudl not go into similarityScore
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments xs [] = [(xs, take (length xs) (repeat '-'))]
optAlignments [] ys = [(take (length ys) (repeat '-'), ys)]
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) (concat [attachHeads x y (optAlignments xs ys),
                                                                          attachHeads x '-' (optAlignments xs (y:ys)),
                                                                          attachHeads '-' y (optAlignments (x:xs) ys)])

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = print (optAlignments string1 string2)

--There are 3 optimal alignments:

--w r i t - e r s
--v i n t n e r -

--w r i - t - e r s
--- v i n t n e r -

--w r i - t - e r s
--v - i n t n e r -

--There were 3 optimal alignments!


