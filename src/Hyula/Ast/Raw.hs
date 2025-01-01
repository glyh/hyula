{-# LANGUAGE NoImplicitPrelude #-}

module Hyula.Ast.Raw where

import Relude

import Hyula.Ast.Base (Atom)

type Symbol = Text

data SeqProp = SeqProp {scoped :: Bool}
  deriving (Show, Eq)

data Exp
  = Atom Atom
  | Var Symbol
  | Seq SeqProp [Exp]
  | If Exp [Exp] [Exp]
  | Match Pat Exp
  | List [Exp]
  | Call Exp [Exp]
  | Abs Symbol Exp
  deriving (Show, Eq)

data Pat
  = Union (NonEmpty Pat)
  | Lit Atom
  | PatList [Pat]
  | Any
  deriving (Show, Eq)
