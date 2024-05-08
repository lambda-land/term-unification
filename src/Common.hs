module Common
    ( Var, Term (..), Judgement
    , GlobalState, LocalState, Unif
    , liftLocal, liftGlobal
    , getMeta, newMeta, setMeta
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
data LocalState = LocalState
  { subst :: Map Var Term
  , metaCounter :: Int
  }

type Backtr g s = StateT s (LogicT (State g))
type Unif = Backtr GlobalState LocalState

liftLocal :: State s a -> Backtr g s a
liftLocal st = do
  s <- State.get
  let !(a, s') = State.runState st s
  State.put s'
  pure a

liftGlobal :: State g a -> Backtr g s a
liftGlobal st = do
  s <- lift $ State.get
  let !(a, s') = State.runState st s
  lift $ State.put s'
  pure a

getMeta :: Var -> Unif (Maybe Term)
getMeta v = liftLocal (State.gets (Map.lookup v . subst))

newMeta :: String -> Unif Var
newMeta pref = liftLocal $ State.state $ \(LocalState subst metaCounter) ->
  let v = pref ++ show metaCounter in
  if Map.member v subst
  then error ("variable already assigned: " ++ v)
  else (v, LocalState subst (metaCounter + 1))

setMeta :: Var -> Term -> Unif ()
setMeta v t = liftLocal $ State.modify $ \(LocalState subst metaCounter) ->
  if Map.member v subst
  then error ("variable already assigned: " ++ v)
  else LocalState (Map.insert v t subst) metaCounter

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
