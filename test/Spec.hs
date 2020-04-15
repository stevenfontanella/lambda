{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Ast
import Reduce hiding (reduce)
import Data.Text (Text)
import Lib (reduce)

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    specify "lambda x.x" $ 
      parseLambdaExpr "lambda x.x" `shouldBe` (Right $ Lambda "x" (Lit "x"))
    specify "(lambda x.x) y" $
      parseLambdaExpr "(lambda x.x) y" `shouldBe` (Right $ Apply (Lambda "x" (Lit "x")) (Lit "y"))
    specify " ( lambda x .  x )  (λ   xyz. abc  )" $
      parseLambdaExpr " ( lambda x .  x )  (λ   xyz. abc  def )" `shouldBe` (Right $ Apply (Lambda "x" $ Lit "x") (Lambda "xyz" (Apply (Lit "abc") (Lit "def"))))
    specify "k combinator" $
      parseLambdaExpr "(lambda x . lambda y . y) a b" `shouldBe` Right (Apply (Apply (Lambda "x" (Lambda "y" (Lit "y"))) (Lit "a")) (Lit "b"))

  describe "reducer" $ do
    specify "id" $ do
      (runReduction $ Apply (Lambda "x" (Lit "x")) (Lit "y")) `shouldBe` (Lit "y")
  describe "e2e" $ do
    specify "k combinator" $ do
      reduce "(lambda x. lambda y . y) a b" `shouldBe` Right (Lit "b")
