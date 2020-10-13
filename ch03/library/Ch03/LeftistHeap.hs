{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch03.LeftistHeap
  ( -- $ex3_1
    module Heaps
  , Heap(Empty, Node)
  , ops
  , insertDirect
  , fromList
  ) where

import Ch03.Heaps as Heaps
import Prelude

-- $ex3_1
--
-- __Exercise 3.1__
--
-- Prove that the right spine of a leftist heap of size @n@ contains at most
-- @floor $ logBase 2 $ n + 1@ elements. (All logarithms in this book are base
-- 2 unless otherwise indicated)
--
-- /Proof:/
--
-- TODO

-- | The data type used in leftist heap implementations.
data Heap a
  = Empty
  | Node Int a (Heap a) (Heap a)
  deriving stock (Eq, Show)

-- | A leftist heap implementation.
ops :: Ops Heap
ops = Ops { empty, isEmpty, insert, merge, findMin, deleteMin } where
  empty :: Heap a
  empty = Empty

  isEmpty :: Heap a -> Bool
  isEmpty = \case
    Empty {} -> True
    Node {} -> False

  insert :: (Ord a) => a -> Heap a -> Heap a
  insert x = merge (Node 1 x Empty Empty)

  merge :: (Ord a) => Heap a -> Heap a -> Heap a
  merge h1 h2 =
    case (h1, h2) of
      (h, Empty) -> h
      (Empty, h) -> h
      (Node _r1 x a1 b1, Node _r2 y a2 b2)
        | x <= y     -> makeNode x a1 $ merge b1 h2
        | otherwise -> makeNode y a2 $ merge h1 b2

  findMin :: Heap a -> Maybe a
  findMin = \case
    Empty -> Nothing
    Node _r x _a _b -> Just x

  deleteMin :: (Ord a) => Heap a -> Maybe (Heap a)
  deleteMin = \case
    Empty -> Nothing
    Node _r _x a b -> Just $ merge a b

makeNode :: a -> Heap a -> Heap a -> Heap a
makeNode x a b
  | rank a >= rank b = Node (rank b + 1) x a b
  | otherwise = Node (rank a + 1) x b a

rank :: Heap a -> Int
rank = \case
  Empty -> 0
  Node r _x _a _b -> r

-- | __Exercise 3.2__
--
-- Define 'insert' directly rather than via a call to 'merge'.
insertDirect :: (Ord a) => a -> Heap a -> Heap a
insertDirect x = \case
  Empty -> makeNode x Empty Empty
  heap@(Node _r y a b)
    | x < y -> makeNode x heap Empty
    | otherwise -> makeNode y a $ insertDirect x b

-- | __Exercise 3.3__
--
-- Implement a function 'fromList' of type @(Ord a) => [a] -> Heap a@ that
-- produces a leftist heap from an unordered list of elements by first
-- converting each element into a singleton heap and then merging the heaps
-- until only one heap remains. Instead of merging the heaps in one
-- right-to-left or left-to-right pass using @foldr@ or @foldl@, merge the heaps
-- in @ceiling $ logBase 2 n)@ passes, where each pass merges adjacent pairs of
-- heaps. Show that 'fromList' takes only /O(n)/ time.
fromList :: forall a. (Ord a) => [a] -> Heap a
fromList = go . fmap singleton where
  go :: [Heap a] -> Heap a
  go heaps =
    case mergePairwise heaps of
      [] -> Empty
      [h1] -> h1
      hs -> go hs

  mergePairwise :: [Heap a] -> [Heap a]
  mergePairwise = \case
    [] -> []
    [h1] -> [h1]
    h1 : h2 : rest -> merge ops h1 h2 : mergePairwise rest

  singleton :: a -> Heap a
  singleton x = makeNode x Empty Empty
