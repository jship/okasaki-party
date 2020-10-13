{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch03.WeightBiasedLeftistHeap
  ( -- $ex3_4
    module Heaps
  , Heap(Empty, Node)
  , ops
  , mergeSinglePass
  ) where

import Ch03.Heaps as Heaps
import Ch03.LeftistHeap (Heap(Empty, Node))
import Prelude
import qualified Ch03.LeftistHeap as LeftistHeap

-- $ex3_4
--
-- __Exercise 3.4__
--
-- Weight-biased leftist heaps are an alternative to leftist heaps that replace
-- the leftist property with the /weight-biased leftist property/: the size of
-- any left child is at least as large as the size of its right sibling.
--
-- a) Prove that the right spine of a weight-biased leftist heap contains at
-- most @floor $ logBase 2 $ n + 1@ elements.
--
-- /Proof:/
--
-- TODO
--
-- b) Modify the leftist heap implementation to obtain weight-biased leftist
-- heaps.
--
-- /See this module's implementation./
--
-- c) Currently, @merge@ operates in two passes: a top-down pass consisting of
-- calls to @merge@, and a bottom-up pass consisting of calls to the helper
-- function @makeNode@. Modify @merge@ for weight-biased leftist heaps to
-- operate in a single, top-down pass.
--
-- /See this module's @mergeSinglePass@ function./
--
-- d) What advantages would the top-down version of @merge@ have in a lazy
-- environment? In a concurrent environment?
--
-- In a lazy environment, we can merge multiple heaps while keeping the overall
-- size up-to-date without actually executing the "meat" of the merging.
--
-- In a concurrent environment, we could exploit parallelism to perform the
-- sub-merges.

-- | A weight-biased leftist heap implementation. This implementation is largely
-- identical to the leftist heap implementation and leverages the same 'Heap'
-- type.
--
-- The two key differences:
--
--   1. The 'Int' in the 'Node' constructor represents the heap's /size/, not
--   its rank. Leftist heap rank is no longer a thing.
--   2. Accordingly, the 'insert' and 'merge' functions merge heaps based on
--   /size/, not their rank.
ops :: Ops Heap
ops = LeftistHeap.ops { insert, merge }
  where
  insert :: (Ord a) => a -> Heap a -> Heap a
  insert x = merge (Node 1 x Empty Empty)

  merge :: (Ord a) => Heap a -> Heap a -> Heap a
  merge h1 h2 =
    case (h1, h2) of
      (h, Empty) -> h
      (Empty, h) -> h
      (Node _s1 x a1 b1, Node _s2 y a2 b2)
        | x <= y    -> makeNode x a1 $ merge b1 h2
        | otherwise -> makeNode y a2 $ merge h1 b2

makeNode :: a -> Heap a -> Heap a -> Heap a
makeNode x a b = go where
  go | sizeA >= sizeB = Node sizeTotal x a b
     | otherwise = Node sizeTotal x b a

  sizeTotal = sizeA + sizeB + 1
  sizeA = size a
  sizeB = size b

size :: Heap a -> Int
size = \case
  Empty -> 0
  Node s _x _a _b -> s

mergeSinglePass :: (Ord a) => Heap a -> Heap a -> Heap a
mergeSinglePass h1 h2 =
  case (h1, h2) of
    (h, Empty) -> h
    (Empty, h) -> h
    (Node s1 x a1 b1, Node s2 y a2 b2)
      | x <= y ->
          if size a1 >= size b1 + s2 then
            Node (s1 + s2) x a1 $ mergeSinglePass b1 h2
          else
            Node (s1 + s2) x (mergeSinglePass b1 h2) a1
      | otherwise ->
          if size a2 >= size b2 + s1 then
            Node (s2 + s1) y a2 $ mergeSinglePass b2 h1
          else
            Node (s2 + s1) y (mergeSinglePass b2 h1) a2
