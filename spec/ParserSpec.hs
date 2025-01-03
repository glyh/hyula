{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ParserSpec (spec) where

import Relude
import Test.Hspec
import Text.Megaparsec

import Hyula.Common (Debug (..))
import Hyula.Parser (Parser, pAtom, pExp, pPat, pTopLevel)
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))

parseExpect :: (Debug a) => Parser a -> Text -> Text -> Expectation
parseExpect parser to_parse expected = do
  let result = runParser (parser <* eof) "<parse expect>" to_parse
  either
    (expectationFailure . errorBundlePretty)
    ((`shouldBe` expected) . dbgShow)
    result

spec :: Spec
spec = do
  describe "Atoms" $ do
    it "I64" $ do
      parseExpect pAtom "1145141919" "1145141919"
    it "F64" $ do
      parseExpect pAtom "3.1415926" "3.1415926"
    it "Bool" $ do
      parseExpect pAtom "true" "true"
      parseExpect pAtom "false" "false"
    it "Char" $ do
      parseExpect pAtom "'w'" "'w'"
      parseExpect pAtom "'\\''" "'\\''"
      parseExpect pAtom "'\\\\'" "'\\\\'"
    it "Unit" $ do
      parseExpect pAtom "()" "()"

  describe "Pattern" $ do
    it "Lit" $ do
      parseExpect pPat "3.0" "3.0"
    it "Any" $ do
      parseExpect pPat "_" "_"
    it "List" $ do
      parseExpect pPat "[_, 2, 3, ]" "[_,2,3]"
    it "Union" $ do
      parseExpect pPat "_ | [1, _]" "_|[1,_]"

  describe "Expression" $ do
    it "Atom" $ do
      parseExpect pExp "1" "1"
    it "Function Call" $ do
      parseExpect pExp "add(1,  2, )" "add(1,2)"
      parseExpect pExp "add( 1  ,  2 , )" "add(1,2)"
      parseExpect pExp "g(1.add(3).mul(4).lol(9, 10),100)" "g(lol(mul(add(1,3),4),9,10),100)"

  describe "Comments" $ do
    it "Complex" $ do
      parseExpect pTopLevel "#|nested #|comments|# just like #|this one|#|#  4  # yep" "4"
      parseExpect pTopLevel "#_#_ add(3, 4) 4 114514 # and you can comment AST" "114514"

  describe "Statements" $ do
    it "If" $ do
      let to_parses =
            [ "if true: 1 else: 2"
            , "if true: 1 else  \n 2 \n end"
            , "if true \n 1 \n else:2"
            , "if true \n 1 \n else  \n 2 \n end"
            ]
      let expected = "if true: (#1) else: (#2)"
      mapM_ (\to_parse -> parseExpect pTopLevel to_parse expected) to_parses
    it "Block" $ do
      parseExpect pTopLevel "1; 2; 3" "(1; 2; 3)"
      parseExpect pTopLevel "do\n 1 \n 2\n 3\n end" "(#1; 2; 3)"
