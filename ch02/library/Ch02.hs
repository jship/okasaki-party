{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ch02
  ( -- * Exercises
    -- ** 2.1
    suffixes
    -- ** 2.2 - 2.5 Preamble
  , Set(Elem, emptySet, insert, member)
  , UnbalancedSet(E, T)
    -- *** 2.2
  , member'
    -- *** 2.3
  , insert'
    -- *** 2.4
  , insert''
    -- *** 2.5.a
  , complete
    -- *** 2.5.b
  , complete'
    -- ** 2.6 Preamble
  , Map(Key, Value, emptyMap, bind, lookup)
  , UnbalancedMap(EM, TM)
    -- *** 2.6
    -- ** Miscellaneous
  , fromList
  , infinite
  , takeDepth
  ) where

import Prelude hiding (lookup, max, min)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- | Exercise 2.1
suffixes :: [a] -> [[a]]
suffixes = \case
  [] -> [[]]
  list@(_ : xs) -> list : suffixes xs

-- 2.2 - 2.5 Preamble

-- | A typeclass for set operations.
class Set t where
  type Elem t

  emptySet :: t
  insert :: Elem t -> t -> t
  member :: Elem t -> t -> Bool

-- | A binary tree type.
data UnbalancedSet a
  = E
  | T (UnbalancedSet a) a (UnbalancedSet a)
  deriving stock (Eq, Show)

instance (Ord a) => Set (UnbalancedSet a) where
  type Elem (UnbalancedSet a) = a

  emptySet :: UnbalancedSet a
  emptySet = E

  member :: a -> UnbalancedSet a -> Bool
  member x = \case
    E -> False
    T a y b
      | x < y -> member x a
      | x > y -> member x b
      | otherwise -> True

  insert :: a -> UnbalancedSet a -> UnbalancedSet a
  insert x = \case
    E -> T E x E
    original@(T a y b)
      | x < y -> T (insert x a) y b
      | x > y -> T a y (insert x b)
      | otherwise -> original

-- | Exercise 2.2
--
-- Rewrite 'member' to take no more than @d+1@ comparisons instead of the
-- worst case of @2d@, where @d@ is the depth of the tree.
member' :: forall a. (Ord a) => a -> UnbalancedSet a -> Bool
member' target = \case
  E -> False
  set@(T _ root _) -> go root set where
    go :: a -> UnbalancedSet a -> Bool
    go candidate = \case
      E -> target == candidate
      T a y b
        | target < y -> go candidate a
        | otherwise -> go y b

-- | Exercise 2.3
--
-- Rewrite 'insert' to not copy the search path when inserting an
-- already-existing element.
insert' :: forall a. (Ord a) => a -> UnbalancedSet a -> UnbalancedSet a
insert' x set = Maybe.fromMaybe set $ go set where
  go :: UnbalancedSet a -> Maybe (UnbalancedSet a)
  go = \case
    E -> Just $ T E x E
    T a y b
      | x < y -> T <$> go a <*> pure y <*> pure b
      | x > y -> T <$> pure a <*> pure y <*> go b
      | otherwise -> Nothing

-- | Exercise 2.4
--
-- Combine ideas from 2.2 and 2.3 in a new implementation of 'insert' to perform
-- no unnecessary copying and use no more than @d+1@ comparisons.
insert'' :: forall a. (Ord a) => a -> UnbalancedSet a -> UnbalancedSet a
insert'' x = \case
  E -> T E x E
  set@(T _ root _) -> Maybe.fromMaybe set $ go root set where
    go :: a -> UnbalancedSet a -> Maybe (UnbalancedSet a)
    go candidate = \case
      E | x == candidate -> Nothing
        | otherwise -> Just $ T E x E
      T a y b
        | x < y -> T <$> go candidate a <*> pure y <*> pure b
        | otherwise -> T <$> pure a <*> pure y <*> go y b

-- | Exercise 2.5.a
--
-- Write a function of type:
-- > complete :: a -> Int -> UnbalancedSet a
--
-- Where @complete x d@ creates a complete binary tree of depth @d@ with @x@
-- stored in every node. This function should run in /O(d)/ time.
--
-- TODO: Isn't this @O(2d)@?
complete :: a -> Int -> UnbalancedSet a
complete x = \case
  0 -> E
  depth
    | depth < 0 -> error "complete: depth must be 1 or greater!"
      -- TODO: Explicitly share the recursive call result?
    | otherwise -> T (complete x $ depth - 1) x (complete x $ depth - 1)

-- | Exercise 2.5.b
--
-- Extend the 'complete' function to create balanced trees of arbitrary size.
-- These trees will not always be complete binary trees, but should be as
-- balanced as possible: for any given node, the two subtrees should differ in
-- size by at most one. This function should run in /O(log n)/ time. (Hint: use
-- a helper function @create2@ that, given a size @m@, creates a pair of trees,
-- one of size @m@ and one of size @m+1@.
--
-- This implementation is kind of tricky!  Here are some iterations written out
-- to illustrate how it works:
--
-- > n == 0:
-- >   (E, T E x E)
--
-- > n == 1:
-- >   odd, so it calls go 0
-- >   a = E
-- >   b = T E x E
-- >     so (T a x a, T b x a) is
-- >       (T E x E, T (T E x E) x E)
--
-- > n == 2:
-- >   even, so it calls go 0
-- >   a = E
-- >   b = T E x E
-- >     so (T b x a, T b x b) is
-- >       (T (T E x E) x E, T (T E x E) x (T E x E))
--
-- > n == 3:
-- >   odd, so it calls go 1
-- >   a = T E x E
-- >   b = T (T E x E) x E
-- >     so (T a x a, T b x a) is
-- >       (T (T E x E) x (T E x E), T (T (T E x E) x E) x (T E x E))
complete' :: forall a. a -> Int -> UnbalancedSet a
complete' x size
  | size < 0 = error "complete': size must be non-negative!"
  | otherwise = fst $ go size where
      -- Remember when reading that this function always returns a pair of
      -- trees, where the first tree is of size n and the second tree is of
      -- size n+1.
      go :: Int -> (UnbalancedSet a, UnbalancedSet a)
      go n
        | n == 0 = (E, T E x E)
        | odd n =
            let (a, b) = go $ (n - 1) `div` 2
             in (T a x a, T b x a)
        | otherwise =
            let (a, b) = go $ (n `div` 2) - 1
             in (T b x a, T b x b)

-- 2.6 Preamble

-- | A typeclass for finite map operations.
class Map t where
  type Key t
  type Value t

  emptyMap :: t
  bind :: Key t -> Value t -> t -> t
  lookup :: Key t -> t -> Maybe (Value t)

-- | Exercise 2.6
data UnbalancedMap k v
  = EM
  | TM (UnbalancedMap k v) k v (UnbalancedMap k v)

instance (Ord k) => Map (UnbalancedMap k v) where
  type Key (UnbalancedMap k v) = k
  type Value (UnbalancedMap k v) = v

  emptyMap :: UnbalancedMap k v
  emptyMap = EM

  bind :: k -> v -> UnbalancedMap k v -> UnbalancedMap k v
  bind k v = \case
    EM -> TM EM k v EM
    (TM a k' v' b)
      | k < k' -> TM (bind k v a) k' v' b
      | k > k' -> TM a k' v' (bind k v b)
      | otherwise -> TM a k v b

  lookup :: k -> UnbalancedMap k v -> Maybe v
  lookup k = \case
    EM -> Nothing
    (TM a k' v b)
      | k < k' -> lookup k a
      | k > k' -> lookup k b
      | otherwise -> Just v

-- Miscellaneous stuff --------------------------------------------------------

-- | Creates an initially-balanced 'UnbalancedSet' from a '[Int]'.
fromList :: [Int] -> UnbalancedSet Int
fromList = \case
  [] -> E
  xs -> T left (ys !! idx) right where
    left = fromList $ take idx ys
    right = fromList $ drop (1 + idx) ys
    idx = length ys `div` 2
    ys = List.nub $ List.sort xs

-- | Creates an infinite, balanced, valid 'UnbalancedSet' - no duplicates and
-- sorted appropriately.
infinite :: UnbalancedSet Rational
infinite = go 0 1 where
  go :: Rational -> Rational -> UnbalancedSet Rational
  go min max = T left x right where
    left = go min x
    right = go x max
    x = (min + max) / 2

-- | Takes @depth@ levels from the 'UnbalancedSet'.
takeDepth :: Int -> UnbalancedSet a -> UnbalancedSet a
takeDepth depth set
  | depth < 1 = E
  | otherwise =
      case set of
        E -> E
        T a x b -> T (takeDepth (depth - 1) a) x (takeDepth (depth - 1) b)

--(++) :: [a] -> [a] -> [a]
--(++) xs ys =
--  case xs of
--    [] -> ys
--    (x : rest) -> x : rest ++ ys
