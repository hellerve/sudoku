-- ghc -O2 sudoku.hs -o sudoku && ./sudoku
{-# LANGUAGE TupleSections #-}
import Control.Applicative (Alternative(..))
import Control.Monad (forM_, when)
import Data.List (transpose, minimumBy, (\\))
import Data.Ord  (comparing)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')

type Board = [[Int]]
digits :: [Int]; digits = [1..9]

row i b = b !! i
col j b = map (!! j) b
box i j b = [ b !! r !! c | r <- [r0..r0+2], c <- [c0..c0+2] ]
  where r0 = (i `div` 3) * 3; c0 = (j `div` 3) * 3

cands b i j =
  let v = b !! i !! j
  in if v /= 0 then [v]
     else digits \\ filter (/=0) (row i b ++ col j b ++ box i j b)

solved :: Board -> Bool
solved = all (all (/= 0))

findMRV :: Board -> ((Int,Int), [Int])
findMRV b = minimumBy (comparing (length . snd))
  [ ((i,j), cands b i j) | i <- [0..8], j <- [0..8], b !! i !! j == 0 ]

set :: Board -> Int -> Int -> Int -> Board
set b i j v = take i b ++ [take j r ++ [v] ++ drop (j+1) r] ++ drop (i+1) b
  where r = b !! i

propagate :: Board -> Maybe Board
propagate b0 = runST $ do
  a <- thawBoard b0
  ok <- fixpoint a
  if not ok then pure Nothing else Just <$> freezeBoard a

sweep :: STUArray s (Int,Int) Int -> ST s (Bool, Bool)
sweep a = do
  changed <- newSTRef False
  ok <- newSTRef True
  forM_ [0..8] $ \i ->
    forM_ [0..8] $ \j -> do
      v <- readArray a (i,j)
      when (v == 0) $ do
        cs <- candidatesST a i j
        case cs of
          []   -> writeSTRef ok False
          [x]  -> writeArray a (i,j) x >> writeSTRef changed True
          _    -> pure ()
  (,) <$> readSTRef changed <*> readSTRef ok

fixpoint :: STUArray s (Int,Int) Int -> ST s Bool
fixpoint a = go True
  where
    go ch = if ch then do
               (ch', ok) <- sweep a
               if not ok then pure False else go ch'
             else pure True

candidatesST :: STUArray s (Int,Int) Int -> Int -> Int -> ST s [Int]
candidatesST a i j = do
  usedR <- mapM (\c -> readArray a (i,c)) [0..8]
  usedC <- mapM (\r -> readArray a (r,j)) [0..8]
  let r0 = (i `div` 3) * 3; c0 = (j `div` 3) * 3
  usedB <- sequence [ readArray a (r,c)
                    | r <- [r0..r0+2], c <- [c0..c0+2] ]
  let used = filter (/=0) (usedR ++ usedC ++ usedB)
  pure (digits \\ used)

thawBoard :: Board -> ST s (STUArray s (Int,Int) Int)
thawBoard b = do
  a <- newArray ((0,0),(8,8)) 0
  forM_ [0..8] $ \i -> forM_ [0..8] $ \j ->
    writeArray a (i,j) (b !! i !! j)
  pure a

freezeBoard :: STUArray s (Int,Int) Int -> ST s Board
freezeBoard a = sequence
  [ sequence [ readArray a (i,j) | j <- [0..8] ] | i <- [0..8] ]

solve :: Board -> Maybe Board
solve b0 = do
  b <- propagate b0
  if solved b then pure b else
    let ((i,j), cs) = findMRV b
    in foldr (<|>) empty [ solve (set b i j v) | v <- cs ]

pretty :: Board -> IO ()
pretty b = mapM_ printRow (zip [0..] b)
  where
    printRow (i, r) = do
      putStrLn . unwords $
        concat [ [show (r !! j)] | j <- [0..8] ]

puzzle :: Board
puzzle =
  [ [3,0,6,5,0,8,4,0,0]
  , [5,2,0,0,0,0,0,0,0]
  , [0,8,7,0,0,0,0,3,1]
  , [0,0,3,0,1,0,0,8,0]
  , [9,0,0,8,6,3,0,0,5]
  , [0,5,0,0,9,0,6,0,0]
  , [1,3,0,0,0,0,2,5,0]
  , [0,0,0,0,0,0,0,7,4]
  , [0,0,5,2,0,6,3,0,0]
  ]

main :: IO ()
main = case solve puzzle of
  Nothing -> putStrLn "unsatisfiable"
  Just b  -> pretty b
