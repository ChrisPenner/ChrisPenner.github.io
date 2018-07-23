#!/usr/bin/env stack
{- stack --resolver lts-11.15 --install-ghc runghc
-}
newtype Sort a = Sort
  { getSorted :: [a]
  } deriving (Show, Eq)

-- So long as the elements can be ordered we can combine two sorted lists using mergeSort
instance (Ord a) => Monoid (Sort a) where
  mempty = Sort []
  mappend (Sort a) (Sort b) = Sort $ mergeSort a b

-- simple merge sort implementation
mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort [] xs = xs
mergeSort xs [] = xs
mergeSort (x:xs) (y:ys)
  | y < x = y : mergeSort (x : xs) ys
mergeSort (x:xs) ys = x : mergeSort xs ys

-- We'll keep the 'Sort' constructor private and expose this smart constructor instead so we can
-- guarantee that every list inside a `Sort` is guaranteed to be sorted.
-- We could use a simple sort function for this, but might as well use mergeSort since 
-- we already wrote it.
toSort :: Ord a => [a] -> Sort a
toSort = foldMap (Sort . pure)

main :: IO ()
main = print (foldMap (toSort . pure) [5, 2, 3, 1, 0, 8])
