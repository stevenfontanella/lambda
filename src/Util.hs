{-# LANGUAGE OverloadedStrings #-}

module Util where

import Ast

import Data.Text (Text)

infixl 9 .$
(.$) = Apply

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

s' :: Expr Text
s' = Lambda "a" (Lambda "b" (Lambda "c" (Apply (Apply (Lit "a") (Lit "c")) (Apply (Lit "b") (Lit "c")))))

k' :: Expr Text
k' = Lambda "d" (Lambda "e" (Lit "d"))

i' :: Expr Text
i' = Lambda "f" $ Lit "f"
