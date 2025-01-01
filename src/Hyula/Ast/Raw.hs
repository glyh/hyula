{-# LANGUAGE NoImplicitPrelude #-}

module Hyula.Ast.Raw where

import Relude

import Hyula.Ast.Base (Atom)

type Symbol = Text

data SeqProp = SeqProp {scoped :: Bool}

data Exp
  = Atom Atom
  | Var Symbol
  | Seq SeqProp [Exp]
  | If Exp [Exp] [Exp]
  | Match Pat Exp
  | List [Exp]
  | Call Exp [Exp]
  | Abs Symbol Exp

data Pat
  = Union (NonEmpty Pat)
  | Lit Atom
  | PatList [Pat]
  | Any
