{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Relude

import Test.Hspec

import Hyula.Ast.Base (Atom (I64))
import Hyula.Ast.Raw (Exp (Atom))
import Hyula.Parser (pTopLevel)
import Text.Megaparsec (runParser)

main :: IO ()
main = hspec $ do
  describe "Parsing source code" $ do
    it "parses atom" $ do
      let result = runParser pTopLevel "<unit test>" "10"
      result `shouldBe` (Right (Atom (I64 10)))
