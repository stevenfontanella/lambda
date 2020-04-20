{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable

import Lib
import Reduce
import Util
import Ast

main :: IO ()
main = do
  let ReduceState{_env=_, _reductions=reductions} = snd $ runReductionWithSteps ((s' .$ ((k' .$ (s' .$ i')) .$ k')) .$ Lit "a" .$ Lit "b")

  forM_ reductions $ \reduction -> do
    print reduction
  -- putStrLn $ unlines $ map show $ toList reductions

  return ()
