{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.State.LocalGlobal where 


import Control.Monad.Trans.State.Lazy 
  ( State
  , state
  , get
  , put
  , modify
  , modify'
  , gets)

class Monad m => LocalGlobal m g s | m -> g, m -> s where
  global :: State g a -> m a
  local :: State s a -> m a


stateG :: LocalGlobal m g s => (g -> (a, g)) -> m a
stateG = global . state
{-# INLINE stateG #-}

stateL :: LocalGlobal m g s => (s -> (a, s)) -> m a
stateL = local . state
{-# INLINE stateL #-}


putG :: LocalGlobal m g s => g -> m ()
putG = global . put
{-# INLINE putG #-}

putL :: LocalGlobal m g s => s -> m ()
putL = local . put
{-# INLINE putL #-}


getG :: LocalGlobal m g s => m g
getG = global get
{-# INLINE getG #-}

getL :: LocalGlobal m g s => m s
getL = local get
{-# INLINE getL #-}


modifyG :: LocalGlobal m g s => (g -> g) -> m ()
modifyG = global . modify
{-# INLINE modifyG #-}

modifyL :: LocalGlobal m g s => (s -> s) -> m ()
modifyL = local . modify
{-# INLINE modifyL #-}


modifyG' :: LocalGlobal m g s => (g -> g) -> m ()
modifyG' = global . modify'
{-# INLINE modifyG' #-}

modifyL' :: LocalGlobal m g s => (s -> s) -> m ()
modifyL' = local . modify'
{-# INLINE modifyL' #-}


getsG :: LocalGlobal m g s => (g -> a) -> m a
getsG = global . gets
{-# INLINE getsG #-}

getsL :: LocalGlobal m g s => (s -> a) -> m a
getsL = local . gets
{-# INLINE getsL #-}

