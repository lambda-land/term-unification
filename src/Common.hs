module Common
    ( Var, Term (..), Judgement
    , GlobalState, LocalState, Unif
    , liftLocal, liftGlobal
    , inspect, inspectDeep
    ) where

import Control.Monad.Trans (lift)

import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (StateT, State)

import Control.Monad.Logic (LogicT)

import Control.Monad (forM_, guard)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Control.Monad.Logic (LogicT, msplit)

type Var = String
data Term = Var Var
          | Term String [Term]

type Judgement = Term

type GlobalState = ()
type LocalState = Map Var Term

type Unif = StateT LocalState (LogicT (State GlobalState))

getMeta :: Var -> Unif (Maybe Term)
getMeta v = liftLocal (State.gets (Map.lookup v))

liftLocal :: State LocalState a -> Unif a
liftLocal st = do
  s <- State.get
  let !(a, s') = State.runState st s
  State.put s'
  pure a

liftGlobal :: State GlobalState a -> Unif a
liftGlobal st = do
  s <- lift $ State.get
  let !(a, s') = State.runState st s
  lift $ State.put s'
  pure a

inspect :: Term -> Unif Term
inspect (Term c ts) = pure (Term c ts)
inspect (Var v) = do
  mb <- getMeta v
  case mb of
    Nothing -> pure (Var v)
    Just t  -> inspect t

inspectDeep :: Term -> Unif Term
inspectDeep t = do
  t' <- inspect t
  case t' of
    Var v     -> pure (Var v)
    Term c ts -> Term c <$> mapM inspectDeep ts
