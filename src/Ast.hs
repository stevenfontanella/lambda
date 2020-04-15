{-# LANGUAGE OverloadedStrings #-}

module Ast where

import Data.Function (on)
import Control.Applicative hiding (many)
import Control.Monad
import Text.Parsec hiding ((<|>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isSpace)

import Debug.Trace

{-
expr = 
  lambda term . expr -- could be another lambda, apply
  atomic atomic atomic ...
  atomic

atomic =
  (expr)
  term

term = some text
-}

data Expr a =
    Lambda a (Expr a)
  | Lit a
  | Apply (Expr a) (Expr a)
  deriving (Show, Eq)

parseAtomicExpr :: Parsec Text () (Expr Text)
parseAtomicExpr = 
      between (char '(' *> spaces) (spaces *> char ')') parseExpr
  <|> parseLit

lambdaChar :: Parsec Text () ()
lambdaChar = do
  string "λ" <|> string "lambda "
  return ()

parseIdentifier :: Parsec Text () Text
parseIdentifier = do
  first <- letter
  rest <- many (letter <|> digit)
  let iden = T.cons first $ T.pack rest
  when (iden `elem` ["λ", "lambda"]) $
    fail ""
  return iden

parseLambda :: Parsec Text () (Expr Text)
parseLambda = do
  lambdaChar
  spaces
  param <- parseIdentifier
  spaces
  char '.'
  spaces
  body <- parseExpr
  return $ Lambda param body

parseLit :: Parsec Text () (Expr Text)
parseLit = 
  Lit <$> parseIdentifier

parseApply :: Parsec Text () (Expr Text)
parseApply = 
  chainl1 (parseAtomicExpr <* spaces) (pure Apply)

parseExpr :: Parsec Text () (Expr Text)
parseExpr = 
  try parseApply <|> parseLambda <|> parseAtomicExpr 

parseLambdaExpr :: Text -> Either ParseError (Expr Text)
parseLambdaExpr = parse (spaces *> parseExpr <* eof) ""
