module Common
    ( Var, Term (..), Judgement
    , GlobalState, LocalState, Unif
    , getMeta, newMeta, setMeta
    , inspect, inspectDeep
    , emptyLocalState
    , metaCounter
    ) where

import Control.Monad.Trans (lift)

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (StateT, State)

import Control.Monad.Logic (LogicT)

import Control.Monad (forM_, guard)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Control.Monad.Logic (LogicT, msplit)

import Control.Monad.Backtrack (Backtr)
import Control.Monad.State.LocalGlobal (local, global)

type Var = String
data Term = Var Var
          | Term String [Term]
          deriving (Eq, Show)
type Judgement = Term

type GlobalState = ()
data LocalState = LocalState
  { subst :: Map Var Term
  , metaCounter :: Int
  }

emptyLocalState :: LocalState
emptyLocalState = LocalState Map.empty 0

type Unif = Backtr GlobalState LocalState

getMeta :: Var -> Unif (Maybe Term)
getMeta v = local (State.gets (Map.lookup v . subst))

newMeta :: String -> Unif Var
newMeta pref = local $ State.state $ \(LocalState subst metaCounter) ->
  let v = pref ++ show metaCounter in
  if Map.member v subst
  then error ("variable already assigned: " ++ v)
  else (v, LocalState subst (metaCounter + 1))

setMeta :: Var -> Term -> Unif ()
setMeta v t = local $ State.modify $ \(LocalState subst metaCounter) ->
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
