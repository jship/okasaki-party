{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Test.Ch02Spec
  ( spec
  ) where

import Ch02
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter 02 Tests" do
    describe "suffixes" do
      it "empty list" do
        suffixes @Int [] `shouldBe` [[]]
      it "book example" do
        suffixes @Int [1, 2, 3, 4] `shouldBe`
          [ [1, 2, 3, 4]
          , [2, 3, 4]
          , [3, 4]
          , [4]
          , []
          ]
    describe "member" do
      it "empty tree" do
        let tree = fromList []
        member 1 tree `shouldBe` False
      it "tree of depth 1" do
        let tree = fromList [1]
        member 0 tree `shouldBe` False
        member 1 tree `shouldBe` True
        member 2 tree `shouldBe` False
      it "tree of depth 2" do
        let tree = fromList [1..3]
        member 0 tree `shouldBe` False
        member 1 tree `shouldBe` True
        member 2 tree `shouldBe` True
        member 3 tree `shouldBe` True
        member 4 tree `shouldBe` False
      it "tree of depth 3" do
        let tree = fromList [1..5]
        member 0 tree `shouldBe` False
        member 1 tree `shouldBe` True
        member 2 tree `shouldBe` True
        member 3 tree `shouldBe` True
        member 4 tree `shouldBe` True
        member 5 tree `shouldBe` True
        member 6 tree `shouldBe` False
    describe "insert" do
      it "insert into empty" do
        let tree = fromList []
        insert 1 tree `shouldBe` T E 1 E
      it "insert into single-element" do
        let tree = fromList [2]
        insert 1 tree `shouldBe` T (T E 1 E) 2 E
        insert 3 tree `shouldBe` T E 2 (T E 3 E)
      it "insert into multi-element" do
        let tree = fromList [1,3,5]
        insert 0 tree `shouldBe` T (T (T E 0 E) 1 E) 3 (T E 5 E)
        insert 2 tree `shouldBe` T (T E 1 (T E 2 E)) 3 (T E 5 E)
        insert 4 tree `shouldBe` T (T E 1 E) 3 (T (T E 4 E) 5 E)
    describe "member'" do
      it "empty tree" do
        let tree = fromList []
        member' 1 tree `shouldBe` False
      it "tree of depth 1" do
        let tree = fromList [1]
        member' 0 tree `shouldBe` False
        member' 1 tree `shouldBe` True
        member' 2 tree `shouldBe` False
      it "tree of depth 2" do
        let tree = fromList [1..3]
        member' 0 tree `shouldBe` False
        member' 1 tree `shouldBe` True
        member' 2 tree `shouldBe` True
        member' 3 tree `shouldBe` True
        member' 4 tree `shouldBe` False
      it "tree of depth 3" do
        let tree = fromList [1..5]
        member' 0 tree `shouldBe` False
        member' 1 tree `shouldBe` True
        member' 2 tree `shouldBe` True
        member' 3 tree `shouldBe` True
        member' 4 tree `shouldBe` True
        member' 5 tree `shouldBe` True
        member' 6 tree `shouldBe` False
    describe "insert'" do
      it "insert' into empty" do
        let tree = fromList []
        insert' 1 tree `shouldBe` T E 1 E
      it "insert' into single-element" do
        let tree = fromList [2]
        insert' 1 tree `shouldBe` T (T E 1 E) 2 E
        insert' 3 tree `shouldBe` T E 2 (T E 3 E)
      it "insert' into multi-element" do
        let tree = fromList [1,3,5]
        insert' 0 tree `shouldBe` T (T (T E 0 E) 1 E) 3 (T E 5 E)
        insert' 2 tree `shouldBe` T (T E 1 (T E 2 E)) 3 (T E 5 E)
        insert' 4 tree `shouldBe` T (T E 1 E) 3 (T (T E 4 E) 5 E)
    describe "insert''" do
      it "insert'' into empty" do
        let tree = fromList []
        insert'' 1 tree `shouldBe` T E 1 E
      it "insert'' into single-element" do
        let tree = fromList [2]
        insert'' 1 tree `shouldBe` T (T E 1 E) 2 E
        insert'' 3 tree `shouldBe` T E 2 (T E 3 E)
      it "insert'' into multi-element" do
        let tree = fromList [1,3,5]
        insert'' 0 tree `shouldBe` T (T (T E 0 E) 1 E) 3 (T E 5 E)
        insert'' 2 tree `shouldBe` T (T E 1 (T E 2 E)) 3 (T E 5 E)
        insert'' 4 tree `shouldBe` T (T E 1 E) 3 (T (T E 4 E) 5 E)
    describe "complete" do
      it "depth 0" do
        complete @Int 42 0 `shouldBe` E
      it "depth 1" do
        complete @Int 42 1 `shouldBe` T E 42 E
      it "depth 2" do
        complete @Int 42 2 `shouldBe` T (T E 42 E) 42 (T E 42 E)
      it "depth 3" do
        complete @Int 42 3 `shouldBe` T (T (T E 42 E) 42 (T E 42 E)) 42 (T (T E 42 E) 42 (T E 42 E))
    describe "complete'" do
      it "size 0" do
        complete' @Int 42 0 `shouldBe` E
      it "size 1" do
        complete' @Int 42 1 `shouldBe` T E 42 E
      it "size 2" do
        complete' @Int 42 2 `shouldBe` T (T E 42 E) 42 E
      it "size 3" do
        complete' @Int 42 3 `shouldBe` T (T E 42 E) 42 (T E 42 E)
      it "size 4" do
        complete' @Int 42 4 `shouldBe` T (T (T E 42 E) 42 E) 42 (T E 42 E)
      it "size 5" do
        complete' @Int 42 5 `shouldBe` T (T (T E 42 E) 42 E) 42 (T (T E 42 E) 42 E)
      it "size 6" do
        complete' @Int 42 6 `shouldBe` T (T (T E 42 E) 42 (T E 42 E)) 42 (T (T E 42 E) 42 E)
      it "size 7" do
        complete' @Int 42 7 `shouldBe` T (T (T E 42 E) 42 (T E 42 E)) 42 (T (T E 42 E) 42 (T E 42 E))
    describe "fromList" do
      it "[1]" do
        fromList [1] `shouldBe` T E 1 E
      it "[1..2]" do
        fromList [1..2] `shouldBe` T (T E 1 E) 2 E
      it "[1..3]" do
        fromList [1..3] `shouldBe` T (T E 1 E) 2 (T E 3 E)
      it "[1..10]" do
        fromList [1..5] `shouldBe`
          T (T (T E 1 E) 2 E)
            3
            (T (T E 4 E) 5 E)
