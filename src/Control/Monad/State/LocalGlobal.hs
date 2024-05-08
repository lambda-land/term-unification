{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.State.LocalGlobal where 


import Control.Monad.State ( State )

class Monad m => LocalGlobal m g s | m -> g, m -> s where
  global :: State g a -> m a
  local :: State s a -> m a

