{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Ch03.BinomialHeapSpec
  ( spec
  ) where

import Ch03.BinomialHeap (Tree(Node), findMinDirect)
import Prelude
import Test.Hspec
import qualified Ch03.BinomialHeap as BinomialHeap

spec :: Spec
spec = do
  describe "Chapter 03 Tests - Binomial Heaps" do
    describe "empty" do
      it "works" do
        empty @Int `shouldBe` []
    describe "isEmpty" do
      it "on empty" do
        isEmpty @Int empty `shouldBe` True
      it "on non-empty" do
        isEmpty @Int (insert 1 empty) `shouldBe` False
    describe "insert" do
      it "on empty" do
        insert @Int 1 empty `shouldBe` [Node 0 1 []]
      it "on size 1" do
        insert @Int 2 (insert 1 empty) `shouldBe`
          [Node 1 1 [Node 0 2 []]]
      it "on size 2" do
        insert @Int 3 (insert 2 $ insert 1 empty) `shouldBe`
          [Node 0 3 [], Node 1 1 [Node 0 2 []]]
      it "on size 3" do
        insert @Int 4 (insert 3 $ insert 2 $ insert 1 empty) `shouldBe`
          [Node 2 1 [Node 1 3 [Node 0 4 []], Node 0 2 []]]
    describe "merge" do
      it "empty with empty" do
        merge @Int empty empty `shouldBe` []
      it "empty with size 1" do
        merge @Int empty (insert 1 empty) `shouldBe` [Node 0 1 []]
      it "size 1 with empty" do
        merge @Int (insert 1 empty) empty `shouldBe` [Node 0 1 []]
      it "size 1 with size 1" do
        merge @Int (insert 2 empty) (insert 1 empty) `shouldBe`
          [Node 1 1 [Node 0 2 []]]
      it "size 1 with size 2" do
        merge @Int (insert 3 empty) (insert 2 $ insert 1 empty) `shouldBe`
          [Node 0 3 [], Node 1 1 [Node 0 2 []]]
      it "size 2 with size 1" do
        merge @Int  (insert 2 $ insert 1 empty) (insert 3 empty) `shouldBe`
          [Node 0 3 [], Node 1 1 [Node 0 2 []]]
      it "size 2 with size 2" do
        merge @Int (insert 4 $ insert 3 empty) (insert 2 $ insert 1 empty)
          `shouldBe`
            [Node 2 1 [Node 1 3 [Node 0 4 []], Node 0 2 []]]
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
        deleteMin @Int (insert 1 empty) `shouldBe` Just []
      it "on size 2" do
        deleteMin @Int (insert 2 $ insert 1 empty) `shouldBe` Just [Node 0 2 []]
    describe "findMinDirect" do
      it "on empty" do
        findMinDirect @Int empty `shouldBe` Nothing
      it "on size 1" do
        findMinDirect @Int (insert 1 empty) `shouldBe` Just 1
      it "on size 2" do
        findMinDirect @Int (insert 2 $ insert 1 empty) `shouldBe` Just 1
  where
  BinomialHeap.Ops
    { empty
    , isEmpty
    , insert
    , merge
    , findMin
    , deleteMin
    } = BinomialHeap.ops
