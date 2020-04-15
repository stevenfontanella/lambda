{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Reduce where

import Ast
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

type Env = Map Text (Expr Text)

newtype ReduceM a = ReduceM {
  getReduceM :: StateT Env Identity a
} deriving (Functor, Applicative, Monad, MonadState Env)

envAdd :: Text -> Expr Text -> Env -> Env 
envAdd = M.insert

envLookup :: Text -> Env -> Maybe (Expr Text)
envLookup = M.lookup

reduce :: Expr Text -> ReduceM (Expr Text)
reduce (Lit x) = maybe (Lit x) id . envLookup x <$> get
reduce (Lambda x body) = Lambda x <$> reduce body
reduce (Apply f x) =
  reduce f >>= \case
    Lambda param body -> do
        modify $ envAdd param x
        reduce body
    lit@(Lit _) -> Apply lit <$> reduce x
    app@(Apply g y) -> (`Apply` x) <$> reduce app

runReduceM :: ReduceM a -> a
runReduceM (ReduceM m) = runIdentity $ evalStateT m M.empty 

runReduction :: Expr Text -> Expr Text
runReduction = runReduceM . reduce
