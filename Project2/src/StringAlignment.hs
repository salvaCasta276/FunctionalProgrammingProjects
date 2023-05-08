-- Salvador Castagnino, Aden McCusker
import Data.List
import Text.Printf

main :: IO ()
main = do
  --printf "The score is %d\n\n" (newSimilarityScore "writers" "vintner")
  outputOptAlignments "writers" "vintner"
  --outputOptAlignments "aferociousmonadatemyhamster" "functionalprogrammingrules"
  --outputOptAlignments "bananrepubliksinvasionsarmestabsadjutant" "kontrabasfiolfodralmakarmästarlärling"

scoreSpace = -1
score(x, '-') = scoreSpace
score('-', x) = scoreSpace
score(x, y) = if x == y then 0 else -1

similarityScore :: String -> String -> Int
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score(x, y),
                                        similarityScore xs (y:ys) + score(x, '-'),
                                        similarityScore (x:xs) ys + score('-', y)]

newSimilarityScore :: String -> String -> Int
newSimilarityScore xs ys = simCache (length xs) (length ys)
  where
    simCache i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]
       
    simEntry :: Int -> Int -> Int
    simEntry i 0 = scoreSpace * i
    simEntry 0 j = scoreSpace * j
    simEntry i j = maximum [simCache (i-1) (j-1) + score(x, y),
                            score(x, '-') + simCache (i-1) j,
                            score(y, '-') + simCache i (j-1)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter ((== maximum (map valueFcn xs)) . valueFcn) xs 

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs [] = [(xs, replicate (length xs) '-')]
optAlignments [] ys = [(replicate (length ys) '-', ys)]
optAlignments (x:xs) (y:ys) = concatMap (\(a, b) -> uncurry attachHeads a (uncurry optAlignments b)) opts
  where
    opts = maximaBy (\(a, b) -> score a + uncurry similarityScore b) [((x, y), (xs, ys)),
                                                                        ((x, '-'), (xs, y:ys)),
                                                                        (('-', y), (x:xs, ys))]

newOptAlignments :: String -> String -> [AlignmentType]
newOptAlignments xs ys = snd (optCache (length xs) (length ys))
  where
    optCache i j = optTable!!i!!j
    optTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]
       
    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry i 0 = (scoreSpace * i, [(reverse (take i (reverse xs)), replicate i '-')])
    optEntry 0 j = (scoreSpace * j, [(replicate j '-', reverse (take j (reverse ys)))])
    optEntry i j = ((maximum . map fst) opts, (concatMap snd . maximaBy fst) opts)
      where
         x = xs!!(length xs - i)
         y = ys!!(length ys - j)
         opts = [(fst (optCache (i-1) (j-1))  + score(x, y), attachHeads x y (snd (optCache (i-1) (j-1)))),
                 (fst (optCache (i-1) j) + score(x, '-'), attachHeads x '-' (snd (optCache (i-1) j))),
                 (fst (optCache i (j-1)) + score('-', y), attachHeads '-' y (snd (optCache i (j-1))))]

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  printf "There are %d optimal alignments:\n\n" (length alignments)
  mapM_ printPair alignments
  printf "There were %d optimal alignments!\n" (length alignments)
    where
      printPair (x, y) = putStrLn (x ++ "\n" ++ y ++ "\n")
      alignments = newOptAlignments string1 string2

--There are 3 optimal alignments:

--w r i t - e r s
--v i n t n e r -

--w r i - t - e r s
--- v i n t n e r -

--w r i - t - e r s
--v - i n t n e r -

--There were 3 optimal alignments!


