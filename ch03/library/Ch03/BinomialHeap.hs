{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ch03.BinomialHeap
  ( module Heaps
  , Heap
  , Tree(Node)
  , ops
  , findMinDirect
  ) where

import Ch03.Heaps as Heaps
import GHC.Exts (IsList)
import Prelude

-- | The data type used in this module's binomial heap implementation.
newtype Heap a = Heap
  { trees :: [Tree a]
  } deriving stock (Eq, Show)
    deriving newtype (IsList)

cons :: Tree a -> Heap a -> Heap a
cons t (Heap ts) = Heap $ t : ts

data Tree a = Node Int a [Tree a]
  deriving stock (Eq, Show)

-- | A binomial heap implementation.
ops :: Ops Heap
ops = Ops { empty, isEmpty, insert, merge, findMin, deleteMin }
  where
  empty :: Heap a
  empty = Heap []

  isEmpty :: Heap a -> Bool
  isEmpty = null . trees

  insert :: (Ord a) => a -> Heap a -> Heap a
  insert x = insTree (Node 0 x [])

  merge :: (Ord a) => Heap a -> Heap a -> Heap a
  merge (Heap ts1) (Heap ts2) =
    case (ts1, ts2) of
      (ts, []) -> Heap ts
      ([], ts) -> Heap ts
      (t1 : rest1, t2 : rest2)
        | rank t1 < rank t2 -> t1 `cons` merge (Heap rest1) (Heap ts2)
        | rank t2 < rank t1 -> t2 `cons` merge (Heap rest2) (Heap ts1)
        | otherwise -> insTree (link t1 t2) $ merge (Heap rest1) (Heap rest2)

  findMin :: (Ord a) => Heap a -> Maybe a
  findMin = fmap (root . fst) . removeMinTree

  deleteMin :: (Ord a) => Heap a -> Maybe (Heap a)
  deleteMin heap = go where
    go = flip fmap (removeMinTree heap) \(Node _r _x ts, h) ->
      merge (Heap $ reverse ts) h

rank :: Tree a -> Int
rank (Node r _x _c) = r

root :: Tree a -> a
root (Node _r x _c) = x

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r1 x1 c1) t2@(Node _r2 x2 c2)
  | x1 <= x2  = Node (r1 + 1) x1 $ t2 : c1
  | otherwise = Node (r1 + 1) x2 $ t1 : c2

insTree :: (Ord a) => Tree a -> Heap a -> Heap a
insTree t1 Heap { trees } =
  case trees of
    [] -> Heap [t1]
    t2 : rest
      | rank t1 < rank t2 -> Heap $ t1 : trees
      | otherwise -> insTree (link t1 t2) $ Heap rest

removeMinTree :: (Ord a) => Heap a -> Maybe (Tree a, Heap a)
removeMinTree (Heap ts) =
  case ts of
    [] -> Nothing
    [t] -> Just (t, Heap [])
    t : rest ->
      case removeMinTree $ Heap rest of
       Nothing -> error "removeMinTree: impossible!"
       Just (t', remainingHeap)
         | root t <= root t' -> Just (t, Heap ts)
         | otherwise -> Just (t', t `cons` remainingHeap)

-- | __Exercise 3.5__
--
-- Define 'findMin' directly rather than via a call to 'removeMinTree'.
findMinDirect :: (Ord a) => Heap a -> Maybe a
findMinDirect (Heap ts) =
  case ts of
    [] -> Nothing
    [t] -> Just $ root t
    t : rest ->
      case findMinDirect $ Heap rest of
       Nothing -> error "removeMinTree: impossible!"
       Just root'
         | root t <= root' -> Just $ root t
         | otherwise -> Just root'
