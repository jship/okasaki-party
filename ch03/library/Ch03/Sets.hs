{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Ch03.Sets
  ( Ops
      ( Ops
      , empty
      , isEmpty
      , insert
      , member
      )
  ) where

import Prelude

-- | A set interface.
data Ops t = Ops
  { empty :: forall a. t a
  , isEmpty :: forall a. t a -> Bool

  , insert :: forall a. (Ord a) => a -> t a -> t a

  , member :: forall a. (Ord a) => a -> t a -> Bool
  }
