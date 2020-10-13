{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Ch03.Heaps
  ( Ops
      ( Ops
      , empty
      , isEmpty
      , insert
      , merge
      , findMin
      , deleteMin
      )
  ) where

import Prelude

-- | A heap interface.
data Ops t = Ops
  { empty :: forall a. t a
  , isEmpty :: forall a. t a -> Bool

  , insert :: forall a. (Ord a) => a -> t a -> t a
  , merge :: forall a. (Ord a) => t a -> t a -> t a

  , findMin :: forall a. (Ord a) => t a -> Maybe a
  , deleteMin :: forall a. (Ord a) => t a -> Maybe (t a)
  }
