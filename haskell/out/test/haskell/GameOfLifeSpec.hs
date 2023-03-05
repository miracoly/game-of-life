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
  describe "nextTicks" $ do
    it "n is 0" $ do
      nextTicks TestData.underpopulation 0 `shouldBe` TestData.underpopulation
    it "all die" $ do
      nextTicks TestData.underpopulation 10 `shouldBe` []
    it "static" $ do
      nextTicks TestData.static 10 `shouldBe` TestData.static
    it "overpopulation" $ do
      nextTicks TestData.overpopulation 2 `shouldBe` TestData.secondNextTickOverpopulation
--    it "birth" $ do
--      nextTick TestData.birth `shouldBe` TestData.nextTickBirth
