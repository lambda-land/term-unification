module Unify
    ( unify
    ) where

import qualified Control.Monad.State.Strict as State

import Control.Monad.Logic (msplit)

import Control.Monad (forM_, guard)
import qualified Data.Map.Strict as Map

import Logic.Proof (Proof (..))

import Common

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = pure False
anyM p (x : xs) = do c <- p x
                     if c then pure True
                          else anyM p xs

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
      setMeta v t
