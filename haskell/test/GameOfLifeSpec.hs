{-# LANGUAGE ScopedTypeVariables #-}

import GameOfLife
import qualified TestData
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "nextTick" $ do
    it "all die" $ do
      nextTick TestData.underpopulation `shouldBe` []
    it "static" $ do
      nextTick TestData.static `shouldBe` TestData.static
    it "overpopulation" $ do
      nextTick TestData.overpopulation `shouldBe` TestData.nextTickOverpopulation
    it "birth" $ do
      nextTick TestData.birth `shouldBe` TestData.nextTickBirth
