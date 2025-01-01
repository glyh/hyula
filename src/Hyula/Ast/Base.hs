{-# LANGUAGE NoImplicitPrelude #-}

module Hyula.Ast.Base (
  Typ (..),
  Atom (..),
) where

import Relude

data Typ
  = TI64
  | Arrow Typ Typ

data Atom
  = I64 Int64
  | F64 Double
  | Bool Bool
  | Char Char
  | Unit
  deriving (Show, Eq)
