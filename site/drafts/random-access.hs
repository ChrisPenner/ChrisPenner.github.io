#!/usr/bin/env stack
{- stack --resolver lts-11.15 --install-ghc runghc
 --package fingertree
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.FingerTree
import Data.Monoid

main :: IO ()
main = print $ atIndex (1) alphabet

-- We need to wrap our primitive value type in a newtype;
-- This allows us to store ANY value in the sequence and helps us avoid
-- some trouble with functional dependencies and orphan instances.
newtype Size a = Size
  { getSize :: a
  } deriving (Show, Eq)

-- Measured is the typeclass we implement to tell the FingerTree how to measure
-- our values into a monoid. In our case every individual element is simply of length '1'
instance Measured (Sum Int) (Size a) where
  measure _ = Sum 1

-- We wrap our values in the 'Size'i wrapper and build a Finger Tree
alphabet :: FingerTree (Sum Int) (Size Char)
alphabet = fromList (fmap Size "abcdefghijklmnopqrstuvwxyz")

-- Get a given index from the tree if it exists
atIndex :: Int -> FingerTree (Sum Int) (Size Char) -> Maybe Char
atIndex n t =
  case viewl . snd $ split (> Sum n) t of
    Size c :< _ -> Just c
    _ -> Nothing
