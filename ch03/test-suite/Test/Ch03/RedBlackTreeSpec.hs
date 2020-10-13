{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ch03.RedBlackTreeSpec
  ( spec
  ) where

import Ch03.RedBlackTree (Set(E, T), Color(R, B), fromOrdListSlow)
import Prelude
import Test.Hspec
import qualified Ch03.RedBlackTree as RedBlackTree

spec :: Spec
spec = do
  describe "Chapter 03 Tests - Red Black Trees" do
    describe "empty" do
      it "works" do
        empty @Int `shouldBe` E
    describe "isEmpty" do
      it "on empty" do
        isEmpty @Int empty `shouldBe` True
      it "on non-empty" do
        isEmpty @Int (insert 1 empty) `shouldBe` False
    describe "member" do
      it "empty tree" do
        let tree = fromOrdListSlow []
        member 1 tree `shouldBe` False
      it "tree of depth 1" do
        let tree = fromOrdListSlow [1]
        member 0 tree `shouldBe` False
        member 1 tree `shouldBe` True
        member 2 tree `shouldBe` False
      it "tree of depth 2" do
        let tree = fromOrdListSlow [1..3]
        member 0 tree `shouldBe` False
        member 1 tree `shouldBe` True
        member 2 tree `shouldBe` True
        member 3 tree `shouldBe` True
        member 4 tree `shouldBe` False
      it "tree of depth 3" do
        let tree = fromOrdListSlow [1..5]
        member 0 tree `shouldBe` False
        member 1 tree `shouldBe` True
        member 2 tree `shouldBe` True
        member 3 tree `shouldBe` True
        member 4 tree `shouldBe` True
        member 5 tree `shouldBe` True
        member 6 tree `shouldBe` False
    describe "insert" do
      it "insert into empty" do
        let tree = fromOrdListSlow []
        insert 1 tree `shouldBe` T B E 1 E
      it "insert into single-element" do
        let tree = fromOrdListSlow [2]
        insert 1 tree `shouldBe` T B (T R E 1 E) 2 E
        insert 3 tree `shouldBe` T B E 2 (T R E 3 E)
      it "insert into multi-element" do
        let tree = fromOrdListSlow [1,3,5]
        insert 0 tree `shouldBe` T B (T B (T R E 0 E) 1 E) 3 (T B E 5 E)
        insert 2 tree `shouldBe` T B (T B E 1 (T R E 2 E)) 3 (T B E 5 E)
        insert 4 tree `shouldBe` T B (T B E 1 E) 3 (T B (T R E 4 E) 5 E)
--    describe "fromOrdList" do
--      it "works" do
--        fromOrdList [] `shouldBe` fromOrdListSlow []
--        fromOrdList [1] `shouldBe` fromOrdListSlow [1]
--        fromOrdList [1..2] `shouldBe` T B E 1 (T R E 2 E)
--        fromOrdList [1..2] `shouldBe` fromOrdListSlow [1..2]
--        fromOrdList [1..3] `shouldBe` fromOrdListSlow [1..3]
--        fromOrdList [1..4] `shouldBe` fromOrdListSlow [1..4]
--        fromOrdList [1..100] `shouldBe` fromOrdListSlow [1..100]
    describe "fromOrdListSlow" do
      it "works" do
        fromOrdListSlow [] `shouldBe` E
        fromOrdListSlow [1] `shouldBe` T B E 1 E
        fromOrdListSlow [1..2] `shouldBe` T B E 1 (T R E 2 E)
        fromOrdListSlow [1..3] `shouldBe` T B (T B E 1 E) 2 (T B E 3 E)
        fromOrdListSlow [1..4] `shouldBe` T B (T B E 1 E) 2 (T B E 3 (T R E 4 E))
        fromOrdListSlow [1..10] `shouldBe`
          T B
            (T B (T B E 1 E) 2 (T B E 3 E))
            4
            (T B
               (T B E 5 E)
               6
               (T R (T B E 7 E) 8 (T B E 9 (T R E 10 E))))
  where
  RedBlackTree.Ops
    { empty
    , isEmpty
    , insert
    , member
    } = RedBlackTree.ops
