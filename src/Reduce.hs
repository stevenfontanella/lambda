{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Reduce where

import Ast
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Foldable
import Data.DList (DList)
import qualified Data.DList as DL
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Debug.Trace

type Env = Map Text (Expr Text)
data ReduceState = ReduceState
 { _env :: Env
 , _reductions :: DList (Expr Text)
 } deriving (Show, Eq)

env :: Lens' ReduceState Env
env f ReduceState{_env=e, ..} = (\e -> ReduceState{_env=e, ..}) <$> f e

reductions :: Lens' ReduceState (DList (Expr Text))
reductions f ReduceState{_reductions=r, ..} = (\r -> ReduceState{_reductions=r, ..}) <$> f r

newtype ReduceT m a = ReduceT {
  getReduceT :: StateT ReduceState m a
} deriving (Functor, Applicative, Monad, MonadState ReduceState)

type ReduceM = ReduceT Identity

envAdd :: Text -> Expr Text -> ReduceM ()
envAdd key value = env %= M.insert key value

envLookup :: Text -> ReduceM (Maybe (Expr Text))
envLookup key = M.lookup key <$> use env 

logReduction :: Expr Text -> ReduceM ()
logReduction expr = reductions %= flip DL.snoc expr

-- TODO: alpha reduction
reduce :: Expr Text -> ReduceM (Expr Text)
reduce (Lit x) = maybe (Lit x) id <$> envLookup x
reduce (Lambda x body) = Lambda x <$> reduceStep body
reduce (Apply f x) =
  reduceStep f >>= \case
    Lambda param body -> do
        envAdd param x
        reduceStep body
    lit@(Lit _) -> liftA2 Apply (reduceStep lit) (reduceStep x)
    app@(Apply g y) -> (`Apply` x) <$> reduceStep app

reduceStep :: Expr Text -> ReduceM (Expr Text)
reduceStep expr = do
  logReduction expr 
  reduce expr
  -- length <$> use reductions >>= \numSteps -> if numSteps < 50 then reduce expr else pure expr

-- TODO: names of these
runReduceT :: ReduceM a -> a 
runReduceT (ReduceT m) = runIdentity $ evalStateT m ReduceState{_env=M.empty, _reductions=DL.empty}

execReduceT :: ReduceM a -> (a, ReduceState)
execReduceT (ReduceT m) = runIdentity $ runStateT m ReduceState{_env=M.empty, _reductions=DL.empty}

runReduction :: Expr Text -> Expr Text
runReduction = runReduceT . reduceStep

runReductionWithSteps :: Expr Text -> (Expr Text, ReduceState)
runReductionWithSteps = execReduceT . reduceStep
