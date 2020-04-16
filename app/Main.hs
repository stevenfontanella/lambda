{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable

import Lib
import Reduce
import Util
import Ast

main :: IO ()
main = do

  let (expr, ReduceState{_env=_, _reductions=reductions}) = runReductionWithSteps ((s .$ i .$ i) .$ Lit "a")
  putStrLn $ unlines $ map show $ toList reductions


  return ()
