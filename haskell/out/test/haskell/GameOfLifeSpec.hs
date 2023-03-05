{-# LANGUAGE ScopedTypeVariables #-}

import GameOfLife
import qualified TestData
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isAlive" $ do
    it "should return true" $ do
      isAlive TestData.static (Position 2 2) `shouldBe` True
      isAlive TestData.static (Position 3 2) `shouldBe` True
      isAlive TestData.static (Position 1 3) `shouldBe` True
      isAlive TestData.static (Position 4 3) `shouldBe` True
      isAlive TestData.static (Position 2 4) `shouldBe` True
      isAlive TestData.static (Position 3 4) `shouldBe` True
    it "should return false" $ do
      isAlive TestData.static (Position 1 1) `shouldBe` False
      isAlive TestData.static (Position 1 2) `shouldBe` False
      isAlive TestData.static (Position 1 4) `shouldBe` False
      isAlive TestData.static (Position 2 1) `shouldBe` False
      isAlive TestData.static (Position 2 3) `shouldBe` False
      isAlive TestData.static (Position 2 3) `shouldBe` False
      isAlive TestData.static (Position 2 5) `shouldBe` False
  describe "isDead" $ do
    it "should return false" $ do
      isDead TestData.static (Position 2 2) `shouldBe` False
      isDead TestData.static (Position 3 2) `shouldBe` False
      isDead TestData.static (Position 1 3) `shouldBe` False
      isDead TestData.static (Position 4 3) `shouldBe` False
      isDead TestData.static (Position 2 4) `shouldBe` False
      isDead TestData.static (Position 3 4) `shouldBe` False
    it "should return true" $ do
      isDead TestData.static (Position 1 1) `shouldBe` True
      isDead TestData.static (Position 1 2) `shouldBe` True
      isDead TestData.static (Position 1 4) `shouldBe` True
      isDead TestData.static (Position 2 1) `shouldBe` True
      isDead TestData.static (Position 2 3) `shouldBe` True
      isDead TestData.static (Position 2 3) `shouldBe` True
      isDead TestData.static (Position 2 5) `shouldBe` True
  describe "nextTick" $ do
    it "all die" $ do
      nextTick TestData.underpopulation `shouldBe` []
    it "static" $ do
      nextTick TestData.static `shouldBe` TestData.static
    it "overpopulation" $ do
      nextTick TestData.overpopulation `shouldBe` TestData.nextTickOverpopulation
    it "birth" $ do
      nextTick TestData.birth `shouldBe` TestData.nextTickBirth
