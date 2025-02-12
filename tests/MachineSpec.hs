module MachineSpec where

import Test.Hspec

add :: Integer -> Integer -> Integer
add a b = a + b

spec :: Spec
spec = do
  describe "Machine code generation" $ do
    it "placehodler test" $ do
      add 0 1 `shouldBe` 1