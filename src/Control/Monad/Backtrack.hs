module Control.Monad.Backtrack where

import Control.Monad.State
import qualified Control.Monad.Trans.State as State
import Control.Monad.Logic

import Control.Monad.State.LocalGlobal

type Backtr g s = StateT s (LogicT (State g))


instance LocalGlobal (Backtr g s) g s where
  global :: State g a -> Backtr g s a
  global st = do
    g <- lift $ lift State.get
    let ~(a,g') = State.runState st g
    lift $ lift $ State.put g'
    return a

  local :: State s a -> Backtr g s a
  local st = do
    s <- State.get
    let ~(a,s') = State.runState st s
    State.put s'
    return a