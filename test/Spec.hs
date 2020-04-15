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
    specify "s combinator" $
      parseLambdaExpr "(lambda x . lambda y . lambda z . x z (y z)) x y z " `shouldBe` Right (Apply (Apply (Apply (Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Lit "x") (Lit "z")) (Apply (Lit "y") (Lit "z")))))) (Lit "x")) (Lit "y")) (Lit "z"))

  describe "reducer" $ do
    specify "id" $ do
      (runReduction $ Apply (Lambda "x" (Lit "x")) (Lit "y")) `shouldBe` (Lit "y")
  describe "e2e" $ do
    specify "k combinator" $ 
      reduce "(lambda x. lambda y . y) a b" `shouldBe` Right (Lit "b")
    specify "s combinator" $ 
      reduce "(lambda x. lambda y . lambda z . x z (y z))" `shouldBe` Right (Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Lit "x") (Lit "z")) (Apply (Lit "y") (Lit "z"))))))
    specify "i combinator" $ 
      reduce "lambda x . x" `shouldBe` Right (Lambda "x" $ Lit "x")
    it "should allow unbound variables" $ do
      reduce "y" `shouldBe` Right (Lit "y")
      reduce "f x" `shouldBe` Right (Apply (Lit "f") (Lit "x"))
