{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ch03.LeftistHeapSpec
  ( spec
  ) where

import Ch03.LeftistHeap (Heap(Empty, Node), fromList, insertDirect)
import Prelude
import Test.Hspec
import qualified Ch03.LeftistHeap as LeftistHeap

spec :: Spec
spec = do
  describe "Chapter 03 Tests - Leftist Heaps" do
    describe "empty" do
      it "works" do
        empty @Int `shouldBe` Empty
    describe "isEmpty" do
      it "on empty" do
        isEmpty @Int empty `shouldBe` True
      it "on non-empty" do
        isEmpty @Int (insert 1 empty) `shouldBe` False
    describe "insert" do
      it "on empty" do
        insert @Int 1 empty `shouldBe` Node 1 1 Empty Empty
      it "on size 1" do
        insert @Int 2 (insert 1 empty) `shouldBe`
          Node 1 1 (Node 1 2 Empty Empty) Empty
      it "on size 2" do
        insert @Int 3 (insert 2 $ insert 1 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "on size 3" do
        insert @Int 4 (insert 3 $ insert 2 $ insert 1 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 (Node 1 4 Empty Empty) Empty)
    describe "merge" do
      it "empty with empty" do
        merge @Int empty empty `shouldBe` Empty
      it "empty with size 1" do
        merge @Int empty (insert 1 empty) `shouldBe` Node 1 1 Empty Empty
      it "size 1 with empty" do
        merge @Int (insert 1 empty) empty `shouldBe` Node 1 1 Empty Empty
      it "size 1 with size 1" do
        merge @Int (insert 2 empty) (insert 1 empty) `shouldBe`
          Node 1 1 (Node 1 2 Empty Empty) Empty
      it "size 1 with size 2" do
        merge @Int (insert 3 empty) (insert 2 $ insert 1 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "size 2 with size 1" do
        merge @Int  (insert 2 $ insert 1 empty) (insert 3 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "size 2 with size 2" do
        merge @Int (insert 4 $ insert 3 empty) (insert 2 $ insert 1 empty)
          `shouldBe`
            Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 (Node 1 4 Empty Empty) Empty)
    describe "findMin" do
      it "on empty" do
        findMin @Int empty `shouldBe` Nothing
      it "on size 1" do
        findMin @Int (insert 1 empty) `shouldBe` Just 1
      it "on size 2" do
        findMin @Int (insert 2 $ insert 1 empty) `shouldBe` Just 1
    describe "deleteMin" do
      it "on empty" do
        deleteMin @Int empty `shouldBe` Nothing
      it "on size 1" do
        deleteMin @Int (insert 1 empty) `shouldBe` Just Empty
      it "on size 2" do
        deleteMin @Int (insert 2 $ insert 1 empty) `shouldBe` Just (Node 1 2 Empty Empty)
    describe "insertDirect" do
      it "on empty" do
        insertDirect @Int 1 empty `shouldBe` Node 1 1 Empty Empty
      it "on size 1" do
        insertDirect @Int 2 (insertDirect 1 empty) `shouldBe`
          Node 1 1 (Node 1 2 Empty Empty) Empty
      it "on size 2" do
        insertDirect @Int 3 (insertDirect 2 $ insertDirect 1 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "on size 3" do
        insertDirect @Int 4 (insertDirect 3 $ insertDirect 2 $ insertDirect 1 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 (Node 1 4 Empty Empty) Empty)
    describe "fromList" do
      it "on empty" do
        fromList @Int [] `shouldBe` Empty
      it "on size 1" do
        fromList @Int [1] `shouldBe` Node 1 1 Empty Empty
      it "on size 2" do
        fromList @Int [1..2] `shouldBe` Node 1 1 (Node 1 2 Empty Empty) Empty
      it "on size 3" do
        fromList @Int [1..3] `shouldBe` Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "on size 4" do
        fromList @Int [1..4] `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) (Node 1 3 (Node 1 4 Empty Empty) Empty)
  where
  LeftistHeap.Ops
    { empty
    , isEmpty
    , insert
    , merge
    , findMin
    , deleteMin
    } = LeftistHeap.ops
