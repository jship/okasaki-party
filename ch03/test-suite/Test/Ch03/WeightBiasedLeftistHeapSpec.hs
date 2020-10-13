{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ch03.WeightBiasedLeftistHeapSpec
  ( spec
  ) where

import Ch03.WeightBiasedLeftistHeap (Heap(Empty, Node), mergeSinglePass)
import Prelude
import Test.Hspec
import qualified Ch03.WeightBiasedLeftistHeap as WeightBiasedLeftistHeap

spec :: Spec
spec = do
  describe "Chapter 03 Tests - Weight-biased Leftist Heaps" do
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
          Node 2 1 (Node 1 2 Empty Empty) Empty
      it "on size 2" do
        insert @Int 3 (insert 2 $ insert 1 empty) `shouldBe`
          Node 3 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "on size 3" do
        insert @Int 4 (insert 3 $ insert 2 $ insert 1 empty) `shouldBe`
          Node 4 1 (Node 2 3 (Node 1 4 Empty Empty) Empty) (Node 1 2 Empty Empty)
    describe "merge" do
      it "empty with empty" do
        merge @Int empty empty `shouldBe` Empty
      it "empty with size 1" do
        merge @Int empty (insert 1 empty) `shouldBe` Node 1 1 Empty Empty
      it "size 1 with empty" do
        merge @Int (insert 1 empty) empty `shouldBe` Node 1 1 Empty Empty
      it "size 1 with size 1" do
        merge @Int (insert 2 empty) (insert 1 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) Empty
      it "size 1 with size 2" do
        merge @Int (insert 3 empty) (insert 2 $ insert 1 empty) `shouldBe`
          Node 3 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "size 2 with size 1" do
        merge @Int  (insert 2 $ insert 1 empty) (insert 3 empty) `shouldBe`
          Node 3 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "size 2 with size 2" do
        merge @Int (insert 4 $ insert 3 empty) (insert 2 $ insert 1 empty)
          `shouldBe`
            Node 4 1 (Node 2 3 (Node 1 4 Empty Empty) Empty) (Node 1 2 Empty Empty)
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
    describe "mergeSinglePass" do
      it "empty with empty" do
        mergeSinglePass @Int empty empty `shouldBe` Empty
      it "empty with size 1" do
        mergeSinglePass @Int empty (insert 1 empty) `shouldBe` Node 1 1 Empty Empty
      it "size 1 with empty" do
        mergeSinglePass @Int (insert 1 empty) empty `shouldBe` Node 1 1 Empty Empty
      it "size 1 with size 1" do
        mergeSinglePass @Int (insert 2 empty) (insert 1 empty) `shouldBe`
          Node 2 1 (Node 1 2 Empty Empty) Empty
      it "size 1 with size 2" do
        mergeSinglePass @Int (insert 3 empty) (insert 2 $ insert 1 empty) `shouldBe`
          Node 3 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "size 2 with size 1" do
        mergeSinglePass @Int  (insert 2 $ insert 1 empty) (insert 3 empty) `shouldBe`
          Node 3 1 (Node 1 2 Empty Empty) (Node 1 3 Empty Empty)
      it "size 2 with size 2" do
        mergeSinglePass @Int (insert 4 $ insert 3 empty) (insert 2 $ insert 1 empty)
          `shouldBe`
            Node 4 1 (Node 2 3 (Node 1 4 Empty Empty) Empty) (Node 1 2 Empty Empty)
  where
  WeightBiasedLeftistHeap.Ops
    { empty
    , isEmpty
    , insert
    , merge
    , findMin
    , deleteMin
    } = WeightBiasedLeftistHeap.ops
