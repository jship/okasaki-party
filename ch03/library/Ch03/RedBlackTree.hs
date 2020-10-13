{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch03.RedBlackTree
  ( module Sets
  , Set(E, T)
  , Color(R, B)
  , ops
  , fromOrdList
  , fromOrdListSlow
  ) where

import Ch03.Sets as Sets
import Prelude
import qualified Data.Foldable as Foldable

-- | The data type used in this red-black tree implementation.
data Set a
  = E
  | T Color (Set a) a (Set a)
  deriving stock (Eq, Show)

setColor :: Color -> Set a -> Set a
setColor color = \case
  E -> E
  T _c a x b -> T color a x b

-- | The red and black in a red-black tree. :)
data Color
  = R
  | B
  deriving stock (Eq, Show)

ops :: Ops Set
ops = Ops { empty, isEmpty, insert, member } where
  empty :: Set a
  empty = E

  isEmpty :: Set a -> Bool
  isEmpty = \case
    E {} -> True
    T {} -> False

  -- TODO: Do candidate style to avoid the unnecessary comparison?
  insert :: (Ord a) => a -> Set a -> Set a
  insert x s = setColor B $ ins s where
    ins = \case
      E -> T R E x E
      set@(T color a y b)
        | x < y -> balance color (ins a) y b
        | x > y -> balance color a y $ ins b
        | otherwise -> set

  member :: forall a. (Ord a) => a -> Set a -> Bool
  member target = \case
    E -> False
    s@(T _color _left root _right) -> go root s where
      go :: a -> Set a -> Bool
      go candidate = \case
        E -> target == candidate
        T _c a y b
          | target < y -> go candidate a
          | otherwise -> go y b

balance :: Color -> Set a -> a -> Set a -> Set a
balance color left root right =
  case (color, left, root, right) of
    (B, (T R (T R a x b) y c), z, d) -> T R (T B a x b) y (T B c z d)
    (B, (T R a x (T R b y c)), z, d) -> T R (T B a x b) y (T B c z d)
    (B, a, x, (T R (T R b y c) z d)) -> T R (T B a x b) y (T B c z d)
    (B, a, x, (T R b y (T R c z d))) -> T R (T B a x b) y (T B c z d)
    (c, a, x, b)                     -> T c a x b

-- | __Exercise 3.9__
--
-- Write a function @fromOrdList@ of type @[Int] -> Set Int@ that converts a
-- sorted list with no duplicates into a red-black tree. your function should
-- run in /O(n)/ time.
fromOrdList :: [Int] -> Set Int
fromOrdList = undefined

fromOrdListSlow :: [Int] -> Set Int
fromOrdListSlow = Foldable.foldl' (flip (insert ops)) E
