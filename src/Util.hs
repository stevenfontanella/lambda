{-# LANGUAGE OverloadedStrings #-}

module Util where

import Ast

import Data.Text (Text)

s :: Expr Text
s = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Lit "x") (Lit "z")) (Apply (Lit "y") (Lit "z")))))

k :: Expr Text
k = Lambda "x" (Lambda "y" (Lit "x"))

i :: Expr Text
i = Lambda "x" $ Lit "x"

true :: Expr Text
true = Lambda "x" (Lambda "y" $ Lit "x")

false :: Expr Text
false = Lambda "x" (Lambda "y" $ Lit "x")
