{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hyula.Ast.Raw where

import Data.Text as T
import Formatting
import Relude hiding (Any)

import Hyula.Ast.Base (Atom)
import Hyula.Common (Debug (..))

type Symbol = Text

data SeqProp = SeqProp {scoped :: Bool}
  deriving (Show, Eq)

data Exp
  = Atom Atom
  | Var Symbol
  | Seq SeqProp [Exp]
  | If Exp Exp Exp
  | Match Pat Exp
  | List [Exp]
  | Call Exp [Exp]
  | Abs Symbol Exp
  deriving (Show, Eq)

instance Debug Exp where
  dbgShow (Atom a) = dbgShow a
  dbgShow (Var sym) = sym
  dbgShow (Seq SeqProp{scoped} exps) =
    let block_lead = if scoped then "#" else ""
     in sformat
          ("(" % stext % stext % ")")
          block_lead
          (T.intercalate "; " $ fmap dbgShow exps)
  dbgShow (If cond then_clause else_clause) =
    sformat ("if " % stext % ": " % stext % " else: " % stext) (dbgShow cond) (dbgShow then_clause) (dbgShow else_clause)
  dbgShow (Match pat rhs) =
    sformat (stext % "=" % stext) (dbgShow pat) (dbgShow rhs)
  dbgShow (List es) =
    sformat ("[" % stext % "]") (T.intercalate "," $ fmap dbgShow es)
  dbgShow (Call f args) =
    sformat (stext % "(" % stext % ")") (dbgShow f) (T.intercalate "," $ fmap dbgShow args)
  dbgShow (Abs _ _) = "TODO"

data Pat
  = Union (NonEmpty Pat)
  | Lit Atom
  | PatList [Pat]
  | Any
  deriving (Eq, Show)

instance Debug Pat where
  dbgShow (Union (p :| ps)) =
    (T.intercalate "|" $ fmap dbgShow (p : ps))
  dbgShow (Lit a) = dbgShow a
  dbgShow (PatList ps) =
    sformat ("[" % stext % "]") (T.intercalate "," $ fmap dbgShow ps)
  dbgShow Any = "_"
