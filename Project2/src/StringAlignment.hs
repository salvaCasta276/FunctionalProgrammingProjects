-- Salvador Castagnino, Aden McCusker
import Data.List

main :: IO ()
main = do
  let score = newSimilarityScore "writers" "vintner"
  putStrLn ("The score is: " ++ show score)
  --let maxLens = maximaBy length ["cs", "efd", "lth", "it"]
  --putStrLn ("The maxLens are: " ++ show maxLens)
  outputOptAlignments "writers" "vintner"

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

similarityScore :: String -> String -> Int
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + if x == y then scoreMatch else scoreMismatch,
                                        similarityScore xs (y:ys) + scoreSpace,
                                        similarityScore (x:xs) ys + scoreSpace]

newSimilarityScore :: String -> String -> Int
newSimilarityScore xs ys = simCache (length xs) (length ys)
  where
    simCache i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry i 0 = scoreSpace * i
    mcsEntry 0 j = scoreSpace * j
    mcsEntry i j = maximum [simCache (i-1) (j-1) + if x == y then scoreMatch else scoreMismatch,
                            scoreSpace + simCache (i-1) j,
                            scoreSpace + simCache i (j-1)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter ((== maximum (map valueFcn xs)) . valueFcn) xs 

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs [] = [(xs, take (length xs) (repeat '-'))]
optAlignments [] ys = [(take (length ys) (repeat '-'), ys)]
optAlignments (x:xs) (y:ys) = concat (map (\(x,y,z) -> (uncurry attachHeads y) (uncurry optAlignments z)) maxims)
  where
    maxims = maximaBy (\(x,y,z) -> x + uncurry similarityScore z) [(if x == y then scoreMatch else scoreMismatch, (x, y), (xs, ys)),
                                                                        (scoreSpace, (x, '-'), (xs, (y:ys))),
                                                                        (scoreSpace, ('-', y), ((x:xs), ys))]

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


