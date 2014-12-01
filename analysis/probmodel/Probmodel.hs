-- {-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude
import Numeric.LinearAlgebra as LA
import Data.MultiSet as MS
import Data.List as DL
import Control.Monad as CM
import System.Random

{-|
  main function
-}
main :: IO ()
main = undefined

{-|
  The 'rand' function generates a random matrix of dimension r x c.
  It takes two arguments of type 'Int' that respectively specify the number of rows and columns.

  >>>rand 3 2
  (3><2)
  [ 0.9037125189340266, 0.25479302427488987
  , 0.6175302786834214,   0.556635366546286
  , 0.5584549817994493,  0.8991678943388014 ]
-}
rand :: Int -> Int -> IO (Matrix Double)
rand r c = do
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

combsWithRep :: Int -> [a] -> [[a]]
combsWithRep k xs = combsBySize xs !! k
 where
   combsBySize = foldr f ([[]] : repeat [])
   f x next = scanl1 (\z n -> DL.map (x:) z ++ n) next

{-|
  The propaguleTypes function generates lists of a given length p of lists of a given length g of type 'Bool'.
  It takes two arguments of type 'Int' that specifies the genome length and the propagule size.

  >>>propaguleTypes 2 2
  [[[True,True],[True,True]],[[True,True],[True,False]],[[True,True],[False,True]],[[True,True],[False,False]],[[True,False],[True,False]],[[True,False],[False,True]],[[True,False],[False,False]],[[False,True],[False,True]],[[False,True],[False,False]],[[False,False],[False,False]]]
-}
propaguleTypes :: Int -> Int -> [[[Bool]]]
-- propaguleTypes p g = CM.replicateM p (boolLists g)
propaguleTypes p g = combsWithRep p (boolLists g)

checkViability :: [[Bool]] -> Bool
checkViability xs = and (DL.map or (transpose xs))

{-|
  The viablePropagules function generatues a list containing
  all viable propagules for a given propagule size and genome length.
  It takes two arguments of type 'Int' that respectively specify the propagule size and the genome length.
-}
viablePropagules :: Int -> Int -> [[[Bool]]]
viablePropagules p g = DL.filter checkViability (propaguleTypes p g)

countPropTypes :: Int -> Int -> [[Occur]]
countPropTypes p g = transpose (DL.map (countGenomesInProps p g) (boolLists g))
  where
    countGenomesInProps p g xs = DL.map (occur xs) (DL.map MS.fromList (viablePropagules p g))

-- CM.replicateM 2 (transpose (DL.map (countPropTypes 3 2) (boolLists 2)))

factorial :: (Num a, Enum a) => a -> a
factorial n = product [n, n-1 .. 1]

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

probAGivenB :: ([Int], [Int]) -> Double
probAGivenB (a,b) =
  let x = DL.map fromIntegral a
      y = DL.map fromIntegral b
  in (((factorial (sum x))) / (product (DL.map factorial x))) * (product (zipWith (**) y x))

-- let pp = DL.map tuplify2 (CM.replicateM 2 (countPropTypes 3 2))
-- DL.map probAGivenB pp
