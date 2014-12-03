-- {-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment
import Prelude
import Numeric.LinearAlgebra as LA
import Data.MultiSet as MS
import Data.List as DL
import Data.List.HT as HT
import Control.Monad as CM
import System.Random

{-|
  main function

  usage:
  probmodel 1 2 2 out.mat out.lab
-}
main :: IO ()
-- main = undefined
main = do
  [n,p,g,matf,labf] <- getArgs
  writeFile matf (dispf 2 (matN'N (read n) (read p) (read g)))
  writeFile labf (show (countPopTypes (read n) (read p) (read g)))

{-|
  The 'randMat' function generates a random matrix of dimension r x c.
  It takes two arguments of type 'Int' that respectively specify the number of rows and columns.

  >>>randMat 3 2
  (3><2)
  [ 0.9037125189340266, 0.25479302427488987
  , 0.6175302786834214,   0.556635366546286
  , 0.5584549817994493,  0.8991678943388014 ]
-}
randMat :: Int -> Int -> IO (Matrix Double)
randMat r c = do
  seed <- randomIO
  return (reshape c $ randomVector seed Uniform (r*c))

{-|
  The boolLists function generates all lists of type 'Bool' of a given length.
  It takes an argument of type 'Int' that specifies the length.

  >>>boolLists 2
  [[True,True],[True,False],[False,True],[False,False]]
-}
boolLists :: Int -> [[Bool]]
boolLists g = CM.replicateM g [True,False]

{-|
  The 'combsWithRep' function takes an argument of type 'Int' and a list and generates all combinations from elements of the list of size given by the first arguments

  >>>combsWithRep 3 [1,2]
  [[1,1,1],[1,1,2],[1,2,2],[2,2,2]]
-}
combsWithRep :: Int -> [a] -> [[a]]
combsWithRep k xs = combsBySize xs !! k
 where
   combsBySize = foldr f ([[]] : repeat [])
   f x = scanl1 (\z n -> DL.map (x:) z ++ n)

{-|
  The 'propaguleTypes' function generates lists of a given length p of lists of a given length g of type 'Bool'.
  It takes two arguments of type 'Int' that specifies the genome length and the propagule size.

  >>>propaguleTypes 2 2
  [[[True,True],[True,True]],[[True,True],[True,False]],[[True,True],[False,True]],[[True,True],[False,False]],[[True,False],[True,False]],[[True,False],[False,True]],[[True,False],[False,False]],[[False,True],[False,True]],[[False,True],[False,False]],[[False,False],[False,False]]]
-}
propaguleTypes :: Int -> Int -> [[[Bool]]]
-- propaguleTypes p g = CM.replicateM p (boolLists g)
propaguleTypes p g = combsWithRep p (boolLists g)

{-|
  The 'checkViability' function determines that there is at least one copy of each bit in each position from a list of lists of type 'Bool'. If so it returns True.

  >>>checkViability ((propaguleTypes 3 2) !! 3)
  True
  >>>checkViability ((propaguleTypes 3 2) !! 15)
  False
-}
checkViability :: [[Bool]] -> Bool
checkViability xs = all or (transpose xs)

{-|
  The 'viablePropagules' function generates a list containing
  all viable propagules for a given propagule size and genome length.
  It takes two arguments of type 'Int' that respectively specify the propagule size and the genome length.

  >>>viablePropagules 3 2
  [[[True,True],[True,True],[True,True]],[[True,True],[True,True],[True,False]],[[True,True],[True,True],[False,True]],[[True,True],[True,True],[False,False]],[[True,True],[True,False],[True,False]],[[True,True],[True,False],[False,True]],[[True,True],[True,False],[False,False]],[[True,True],[False,True],[False,True]],[[True,True],[False,True],[False,False]],[[True,True],[False,False],[False,False]],[[True,False],[True,False],[False,True]],[[True,False],[False,True],[False,True]],[[True,False],[False,True],[False,False]]]
-}
viablePropagules :: Int -> Int -> [[[Bool]]]
viablePropagules p g = DL.filter checkViability (propaguleTypes p g)

{-|
  The 'countPropTypes' function generates a list containing lists
  counting the number of each genotype in each viable propagule type.
  It takes two arguments of type 'Int' that respectively specify the propagule
  size and the genome length.

  >>>countPropTypes 3 2
  [[3,0,0,0],[2,1,0,0],[2,0,1,0],[2,0,0,1],[1,2,0,0],[1,1,1,0],[1,1,0,1],[1,0,2,0],[1,0,1,1],[1,0,0,2],[0,2,1,0],[0,1,2,0],[0,1,1,1]]
-}
countPropTypes :: Int -> Int -> [[Occur]]
countPropTypes p g = transpose (DL.map (countGenomesInProps p g) (boolLists g))
  where
    countGenomesInProps p g xs = DL.map (occur xs . MS.fromList) (viablePropagules p g)

{-|
  Standard factorial function
-}
factorial :: (Num a, Enum a) => a -> a
factorial n = product [n, n-1 .. 1]

{-|
  The 'probAGivenB' function generates the conditional unnormalized
  probability weights of transitioning from one propagule type to another.

  >>>let pp = CM.replicateM 2 (countPropTypes 3 2)
  [[[2,0,0,0],[2,0,0,0]],[[2,0,0,0],[1,1,0,0]],[[2,0,0,0],[1,0,1,0]],[[2,0,0,0],[1,0,0,1]],[[2,0,0,0],[0,1,1,0]],[[1,1,0,0],[2,0,0,0]],[[1,1,0,0],[1,1,0,0]],[[1,1,0,0],[1,0,1,0]],[[1,1,0,0],[1,0,0,1]],[[1,1,0,0],[0,1,1,0]],[[1,0,1,0],[2,0,0,0]],[[1,0,1,0],[1,1,0,0]],[[1,0,1,0],[1,0,1,0]],[[1,0,1,0],[1,0,0,1]],[[1,0,1,0],[0,1,1,0]],[[1,0,0,1],[2,0,0,0]],[[1,0,0,1],[1,1,0,0]],[[1,0,0,1],[1,0,1,0]],[[1,0,0,1],[1,0,0,1]],[[1,0,0,1],[0,1,1,0]],[[0,1,1,0],[2,0,0,0]],[[0,1,1,0],[1,1,0,0]],[[0,1,1,0],[1,0,1,0]],[[0,1,1,0],[1,0,0,1]],[[0,1,1,0],[0,1,1,0]]]
  >>>let pab = DL.map probAGivenB pp
  [4.0,1.0,1.0,1.0,0.0,0.0,2.0,0.0,0.0,0.0,0.0,0.0,2.0,0.0,0.0,0.0,0.0,0.0,2.0,0.0,0.0,0.0,0.0,0.0,2.0]
  >>>let ppab = [x / (sum pab) | x <- pab]
-}
probAGivenB :: [[Int]] -> Double
probAGivenB [a,b] =
  let x = DL.map fromIntegral a
      y = DL.map fromIntegral b
  in factorial (sum x) / product (DL.map factorial x) * product (zipWith (**) y x)
probAGivenB _ = error "Must input list with exactly two lists"

probDistAB :: Int -> Int -> [([[Occur]], Double)]
probDistAB p g =
  let pp = CM.replicateM 2 (countPropTypes p g)
      pab = DL.map probAGivenB pp
  in zip pp [x / sum pab | x <- pab]

{-|
  The 'countPopTypes' function generates a list containing lists
  counting the number of each genotype in each viable propagule type.
  It takes two arguments of type 'Int' that respectively specify the propagule
  size and the genome length.

  >>>countPopTypes 3 2 2
  [[3,0,0,0,0],[2,1,0,0,0],[2,0,1,0,0],[2,0,0,1,0],[2,0,0,0,1],[1,2,0,0,0],[1,1,1,0,0],[1,1,0,1,0],[1,1,0,0,1],[1,0,2,0,0],[1,0,1,1,0],[1,0,1,0,1],[1,0,0,2,0],[1,0,0,1,1],[1,0,0,0,2],[0,3,0,0,0],[0,2,1,0,0],[0,2,0,1,0],[0,2,0,0,1],[0,1,2,0,0],[0,1,1,1,0],[0,1,1,0,1],[0,1,0,2,0],[0,1,0,1,1],[0,1,0,0,2],[0,0,3,0,0],[0,0,2,1,0],[0,0,2,0,1],[0,0,1,2,0],[0,0,1,1,1],[0,0,1,0,2],[0,0,0,3,0],[0,0,0,2,1],[0,0,0,1,2],[0,0,0,0,3]]
-}
countPopTypes :: Int -> Int -> Int -> [[Occur]]
countPopTypes n p g = transpose (DL.map (countPropsInPops n p g) (countPropTypes p g))
  where
    countPropsInPops n p g xs = DL.map (occur xs . MS.fromList) (combsWithRep n (countPropTypes p g))

probBGivenN :: Int -> Int -> Int -> [([[Occur]], Double)]
probBGivenN n p g =
  let propt = countPropTypes p g
      pab = probDistAB p g
      ppab = HT.sliceVertical (length propt) (DL.map snd pab)
      popt = countPopTypes n p g
      popprob = [(x,y) | x<-popt, y<-ppab]
      multTupleList (x,y) = zipWith (*) (DL.map fromIntegral x) y
      normvec xs = [x / sum xs | x <- xs]
      probvec = DL.map normvec (HT.sliceVertical (length propt) (DL.map (sum . multTupleList) popprob))
  in zip (DL.map reverse (sequence [popt,propt])) (concat probvec)

probN'GivenN :: [Double] -> [[Int]] -> Double
probN'GivenN pan [m',m] =
  let n' = DL.map fromIntegral m'
      n = DL.map fromIntegral m
  in factorial (sum n') / product (DL.map factorial n') * product (zipWith (**) pan n')
probN'GivenN _ _ = error "Must input list with exactly two lists"

probDistN'N :: Int -> Int -> Int -> [Double]
probDistN'N n p g =
  let propt = countPropTypes p g
      popt = countPopTypes n p g
      pbn = HT.sliceVertical (length propt) (DL.map snd (probBGivenN n p g))
      nn = DL.map reverse (CM.replicateM 2 popt)
  in zipWith probN'GivenN (DL.concatMap (replicate (length popt)) pbn) nn

-- disp :: Matrix Double -> IO ()
-- disp = putStr . dispf 2

matN'N :: Int -> Int -> Int -> Matrix Double
matN'N n p g =
  let dim = length (countPopTypes n p g)
  in trans ((dim><dim) (probDistN'N n p g) :: Matrix Double)
