{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hyula.Ast.Base (
  Typ (..),
  Atom (..),
) where

import Formatting
import Relude

import Hyula.Common (Debug (..))

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

instance Debug Atom where
  dbgShow (I64 i) = show i
  dbgShow (F64 f) = show f
  dbgShow (Bool True) = "true"
  dbgShow (Bool False) = "false"
  dbgShow (Char '\\') = "'\\\\'"
  dbgShow (Char '\'') = "'\\''"
  dbgShow (Char c) = sformat ("'" % char % "'") c
  dbgShow Unit = "()"
