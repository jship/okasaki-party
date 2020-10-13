{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ch03.BinomialHeapTopLevelRank
  ( -- $ex3_6
    module Heaps
  , Heap
  , Tree(Node)
  , ops
  ) where

import Ch03.Heaps as Heaps
import GHC.Exts (IsList)
import Prelude

-- $ex3_6
--
-- __Exercise 3.6__
--
-- Most of the rank annotations in this representation of binomial heaps are
-- redundant because we know that the children of a node of rank @r@ have ranks
-- @r - 1, .., 0@. Thus, we can remove the rank annotations from each node and
-- instead pair each tree at the top-level with its rank, i.e.,
--
-- > newtype Heap a = Heap { trees :: [(Int, Tree a)] }
--
-- Reimplement binomial heaps with this new representation.

-- | The data type used in this module's binomial heap implementation.
newtype Heap a = Heap
  { trees :: [(Int, Tree a)]
  } deriving stock (Eq, Show)
    deriving newtype (IsList)

cons :: (Int, Tree a) -> Heap a -> Heap a
cons pair (Heap pairs) = Heap $ pair : pairs

data Tree a = Node a [Tree a]
  deriving stock (Eq, Show)

-- | A binomial heap implementation with less redundant rank storage.
ops :: Ops Heap
ops = Ops { empty, isEmpty, insert, merge, findMin, deleteMin }
  where
  empty :: Heap a
  empty = Heap []

  isEmpty :: Heap a -> Bool
  isEmpty = null . trees

  insert :: (Ord a) => a -> Heap a -> Heap a
  insert x = insTree (0, Node x [])

  merge :: (Ord a) => Heap a -> Heap a -> Heap a
  merge (Heap ts1) (Heap ts2) =
    case (ts1, ts2) of
      (ts, []) -> Heap ts
      ([], ts) -> Heap ts
      ((r1, t1) : rest1, (r2, t2) : rest2)
        | r1 < r2 -> (r1, t1) `cons` merge (Heap rest1) (Heap ts2)
        | r2 < r1 -> (r2, t2) `cons` merge (Heap rest2) (Heap ts1)
        | otherwise -> insTree (r1 + 1, (link t1 t2)) $ merge (Heap rest1) (Heap rest2)

  findMin :: (Ord a) => Heap a -> Maybe a
  findMin = fmap (root . snd . fst) . removeMinTree

  deleteMin :: (Ord a) => Heap a -> Maybe (Heap a)
  deleteMin heap = go where
    -- This is probably right? ¯\_(ツ)_/¯
    go = flip fmap (removeMinTree heap) \((_r, Node _x ts), h) ->
      merge (Heap $ zip [0..] $ reverse ts) h

root :: Tree a -> a
root (Node x _c) = x

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
  | x1 <= x2  = Node x1 $ t2 : c1
  | otherwise = Node x2 $ t1 : c2

insTree :: (Ord a) => (Int, Tree a) -> Heap a -> Heap a
insTree (r1, t1) Heap { trees } =
  case trees of
    [] -> Heap [(r1, t1)]
    (r2, t2) : rest
      | r1 < r2 -> Heap $ (r1, t1) : trees
      | otherwise -> insTree (r1 + 1, link t1 t2) $ Heap rest

removeMinTree :: (Ord a) => Heap a -> Maybe ((Int, Tree a), Heap a)
removeMinTree (Heap ts) =
  case ts of
    [] -> Nothing
    [pair] -> Just (pair, Heap [])
    (r, t) : rest ->
      case removeMinTree $ Heap rest of
       Nothing -> error "removeMinTree: impossible!"
       Just ((r', t'), remainingHeap)
         | root t <= root t' -> Just ((r, t), Heap ts)
         | otherwise -> Just ((r', t'), (r, t) `cons` remainingHeap)
