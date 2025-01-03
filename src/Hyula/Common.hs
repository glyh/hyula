module Hyula.Common (Debug (..)) where

import Relude

class Debug a where
  dbgShow :: a -> Text
