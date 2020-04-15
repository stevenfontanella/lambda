module Lib where

import Ast
import Reduce (runReduction)

import Text.Parsec (ParseError)
import Data.Text

reduce :: Text -> Either ParseError (Expr Text)
reduce =
  fmap runReduction . parseLambdaExpr
