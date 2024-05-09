module Control.Monad.Yield (YieldT) where

import Control.Monad.Trans.Class

import Control.Monad (ap, join)

newtype YieldT m a = YieldT { unYieldT :: forall r. (m r -> r) -> (a -> r) -> m r }

yield :: Applicative m => YieldT m ()
yield = YieldT (\yld ret -> pure (yld (pure (ret ()))))

ypure :: Applicative m => a -> YieldT m a
ypure x = YieldT (\_ ret -> pure (ret x))

yreflect :: Applicative m => Either a (YieldT m a) -> YieldT m a
yreflect (Left  a)          = ypure a
yreflect (Right (YieldT y)) = YieldT (\yld ret -> pure (yld (y yld ret)))

yabsorb :: Monad m => m (YieldT m a) -> YieldT m a
yabsorb act = YieldT (\yld ret -> act >>= \(YieldT f) -> f yld ret)

ysplit :: Monad m => YieldT m a -> m (Either a (YieldT m a))
ysplit (YieldT f) = f (Right . yabsorb . fmap yreflect) Left

instance Functor (YieldT m) where
  fmap f (YieldT g) = YieldT h
    where
      h yld ret = g yld (ret . f)

instance Monad m => Applicative (YieldT m) where
  pure = ypure
  (<*>) = ap

instance Monad m => Monad (YieldT m) where
  return = pure
  YieldT f >>= k = YieldT g
    where
      g yld ret = join (f (fmap yld) (\a -> unYieldT (k a) yld ret))

instance MonadTrans YieldT where
  lift act = YieldT (\_ ret -> fmap ret act)
