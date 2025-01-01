module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Relude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
    it "fmap is just map for lists" $ do
      map (* 2) ([1 .. 4] :: [Int]) `shouldBe` fmap (* 2) [1 .. 4]

-- Learn you a Haskell Chp. 8
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id
