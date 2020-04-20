{-# LANGUAGE OverloadedStrings #-}

import qualified Data.DList as DL
import qualified Data.Map as M

import Test.Hspec
import Ast
import Reduce hiding (reduce)
import Data.Text (Text)
import Lib (reduce)
import Util

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    specify "lambda x.x" $ 
      parseLambdaExpr "lambda x.x" `shouldBe` Right i
    specify "(lambda x.x) y" $
      parseLambdaExpr "(lambda x.x) y" `shouldBe` Right (i .$ Lit "y")
    specify " ( lambda x .  x )  (λ   xyz. abc  )" $
      parseLambdaExpr " ( lambda x .  x )  (λ   xyz. abc  def )" `shouldBe` (Right $ Apply (Lambda "x" $ Lit "x") (Lambda "xyz" (Apply (Lit "abc") (Lit "def"))))
    specify "k combinator" $
      parseLambdaExpr "(lambda x . lambda y . x) a b" `shouldBe` Right (k .$ Lit "a" .$ Lit "b")
    specify "s combinator" $
      parseLambdaExpr "(lambda x . lambda y . lambda z . x z (y z)) x y z " `shouldBe` Right (s .$ Lit "x" .$ Lit "y" .$ Lit "z")

  describe "reducer" $ do
    specify "id" $ do
      (runReduction $ Apply (Lambda "x" (Lit "x")) (Lit "y")) `shouldBe` (Lit "y")
  describe "e2e" $ do
    specify "k combinator" $ 
      reduce "(lambda x. lambda y . x) a b" `shouldBe` Right (Lit "a")
    specify "s combinator" $ 
      reduce "(lambda x. lambda y . lambda z . x z (y z))" `shouldBe` Right s
    specify "i combinator" $ 
      reduce "lambda x . x" `shouldBe` Right (Lambda "x" $ Lit "x")
    it "should allow unbound variables" $ do
      reduce "y" `shouldBe` Right (Lit "y")
      reduce "f x" `shouldBe` Right (Apply (Lit "f") (Lit "x"))
    specify "S I I" $
      runReduction ((s .$ i .$ i) .$ Lit "a") `shouldBe` (Lit "a" .$ Lit "a")
    specify "S (K ( S I )) K" $
      runReduction ((s' .$ ((k' .$ (s' .$ i')) .$ k')) .$ Lit "a" .$ Lit "b") `shouldBe` (Lit "b" .$ Lit "a")

    -- specify "S K" $
    --   runReduction (s .$ k) `shouldBe` k
