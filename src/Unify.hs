module Unify
    ( Var, Term
    , GlobalState, LocalState, Unif
    , liftLocal, liftGlobal
    , inspect, inspectDeep
    , unify
    ) where

import Control.Monad.Trans (lift)

import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (StateT, State)

import Control.Monad.Logic (LogicT, msplit)

import Control.Monad (forM_, guard)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Logic.Proof (Proof (..))

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = pure False
anyM p (x : xs) = do c <- p x
                     if c then pure True
                          else anyM p xs

type Var = String
data Term = Var Var
          | Term String [Term]

type GlobalState = ()
type LocalState = Map Var Term

type Unif = StateT LocalState (LogicT (State GlobalState))

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

getMeta :: Var -> Unif (Maybe Term)
getMeta v = liftLocal (State.gets (Map.lookup v))

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

occurs :: Var -> Term -> Unif Bool
occurs v t = do
  t' <- inspect t
  case t' of
    Var v'    -> pure (v == v')
    Term _ ts -> anyM (occurs v) ts

unify :: Term -> Term -> Unif ()
unify t1 t2 = do
  t1' <- inspect t1
  t2' <- inspect t2
  case (t1', t2') of
    (Term s1 ts1, Term s2 ts2) -> do
      guard (s1 == s2 && length ts1 == length ts2)
      forM_ (zip ts1 ts2) (uncurry unify)
    (Var v1, Var v2) | v1 == v2 -> pure ()
    (Var v, t) -> replace v t
    (t, Var v) -> replace v t

  where
    replace :: Var -> Term -> Unif ()
    replace v t = do
      -- occurs check
      guard . not =<< occurs v t
      liftLocal (State.modify' (Map.insert v t))
