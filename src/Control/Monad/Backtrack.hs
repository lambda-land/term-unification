module Control.Monad.Backtrack where

import Control.Monad
import Control.Applicative ( Alternative(..) )
import Control.Monad.State
import qualified Control.Monad.Trans.State as State
import Control.Monad.Logic

import Control.Monad.State.LocalGlobal

newtype Backtr g s a = Backtr { unBacktr :: StateT s (LogicT (State g)) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , MonadLogic)

allResults :: Backtr g s a -> s -> g -> ([(a, s)], g)
allResults bt s1 g1 = runState (observeAllT (runStateT (unBacktr bt) s1)) g1

manyResults :: Int -> Backtr g s a -> s -> g -> ([(a,s)], g)
manyResults n bt s1 g1 = runState (observeManyT n (runStateT (unBacktr bt) s1)) g1

backtr :: StateT s (LogicT (State g)) a -> Backtr g s a
backtr = Backtr


instance LocalGlobal (Backtr g s) g s where
  global :: State g a -> Backtr g s a
  global st = Backtr $ lift $ state $ runState st
  -- global = lift . lift 
  -- global st = backtr $ do
  --   g <- lift $ lift State.get
  --   let ~(a,g') = State.runState st g
  --   lift $ lift $ State.put g'
  --   return a

  local :: State s a -> Backtr g s a
  local st = Backtr $ state $ runState st
  -- local st = backtr $ do
  --   s <- State.get
  --   let ~(a,s') = State.runState st s
  --   State.put s'
  --   return a

interleaveMany :: MonadLogic m => [m a] -> m a
interleaveMany = step [] where
  step []  []       = empty
  step ts' []       = step [] (reverse ts')
  step ts' (t : ts) = do
    mb <- msplit t
    case mb of
      Nothing      -> step ts' ts
      Just (a, t') -> pure a <|> step (t' : ts') ts
{-# INLINE interleaveMany #-}

cycleM :: MonadLogic m => [m a] -> m a
cycleM = interleaveMany
{-# INLINE cycleM #-}

-- Alternative implementation for cycleM
-- cycleM' :: MonadLogic m => [m a] -> m a
-- cycleM' [] = empty
-- -- cycleM' (m:ms) = msplit m >>= maybe (cycleM' ms) (\(a, m') -> pure a <|> cycleM' (ms ++ [m'])) mb
-- cycleM' (m:ms) = do mb <- msplit m
--                     case mb of
--                       Nothing      -> cycleM' ms
--                       Just (a, m') -> pure a <|> cycleM' (ms ++ [m'])


-- | Choose one of the given values non-deterministically.
choose :: MonadLogic m => [a] -> m a
choose = interleaveMany . map pure
{-# INLINE choose #-}

interSequence :: MonadLogic m => [m a] -> m [a]
interSequence [] = pure []
interSequence (m:ms) = m >>|| (\a -> interSequence ms >>|| \as -> pure (a:as))


alternateMany :: MonadLogic m => [a] -> m a
alternateMany = foldr interleave empty . map pure

msplitM :: MonadLogic m => m a -> m a -> m (a, m a)
msplitM m m0 = msplit m >>= maybe (msplit m0 >>= maybe empty pure) pure

vertical :: MonadLogic m => m (m a) -> m a
vertical mma = do
  (ma, mma') <- msplitM mma empty
  (a, ma') <- msplitM ma (vertical mma')
  pure a <|> vertical (mma' <|> pure ma')


horizontal :: MonadLogic m => m (m a) -> m a
horizontal mma = do
  (ma, mma') <- msplitM mma empty
  (a,ma') <- msplitM ma (horizontal mma')
  pure a <|> horizontal (pure ma' <|> mma')


-- transpose :: MonadLogic m => m (m a) -> m (m a)

roundRobin :: MonadLogic m => [m a] -> m a
roundRobin [] = empty
roundRobin (m:ms) = do
  (a, m') <- msplitM m (roundRobin ms)
  pure a <|> roundRobin (ms ++ [m'])


-- same as >>=
(>>==) :: MonadLogic m => m a -> (a -> m b) -> m b
-- m >>== f = horizontal $ fmap f m
m >>== f = do
  (a, m') <- msplitM m empty
  f a <|> (m' >>== f)


-- same as >>-
(>>--) :: MonadLogic m => m a -> (a -> m b) -> m b
m >>-- f = do
  (a, m') <- msplitM m empty
  interleave (f a) (m' >>-- f)


(>>||) :: MonadLogic m => m a -> (a -> m b) -> m b
m >>|| f = vertical $ fmap f m
-- m >>|| f = do
--   (a, m') <- msplitM m empty
--   vertical (pure (f a) <|> pure (m' >>|| f))

(>>*-) :: MonadLogic m => m a -> (a -> m b) -> m b
a >>*- k = step [] [] (Just a) where
  -- step :: MonadLogic m => [m b] -> [m b] -> Maybe (m a) -> m b
  step strip' (nextB : strip) nextA = do
    mb <- msplit nextB
    case mb of
      Nothing          ->            step           strip'  strip nextA
      Just (b, nextB') -> pure b <|> step (nextB' : strip') strip nextA
  step []     [] Nothing =
    empty
  step strip' [] Nothing =
    step [] (reverse strip') Nothing
  step strip' [] (Just nextA) = do
    mb <- msplit nextA
    case mb of
      Nothing          -> step []       (reverse strip') Nothing
      Just (a, nextA') -> step [] (k a : reverse strip') (Just nextA')


-- branchOn :: (g -> s -> [(a,s)]) -> Backtr g s a
-- branchOn f = undefined

-- writeN (n,c) = (concatMap (const "    ") [4..n]) ++ show n ++ " " ++ show c

-- tes1 :: IO ()
-- tes1 = mapM_ putStrLn . map fst . fst $
--   allResults (choose [5, 6, 7, 8]     >>= \n ->
--               choose (take n ['A'..]) >>= \c ->
--               return $ writeN (n, c))
--              () ()

-- tes2 :: IO ()
-- tes2 = mapM_ putStrLn . map fst . fst $
--   allResults (choose [5, 6, 7, 8]     >>- \n ->
--               choose (take n ['A'..]) >>- \c ->
--               return $ writeN (n, c))
--              () ()


-- tes3 = mapM_ putStrLn . map fst . fst $
--   allResults (choose [5, 6, 7, 8]     >>*- \n ->
--               choose (take n ['A'..]) >>*- \c ->
--               return $ writeN (n, c))
--              () ()

-- tes4 = mapM_ putStrLn . map fst . fst $
--   allResults (choose [5, 6, 7, 8]     >>|| \n ->
--               choose (take n ['A'..]) >>|| \c ->
--               return $ writeN (n, c))
--              () ()

-- tes'1 = mapM_ putStrLn . map fst . fst $
--   manyResults 20 (choose [5..]     >>|| \n ->
--               choose (take n ['A'..]) >>|| \c ->
--               return $ writeN (n, c))
--              () ()

-- tes'2 = mapM_ putStrLn . map fst . fst $
--   manyResults 20 (choose [5..]     >>*- \n ->
--               choose (take n ['A'..]) >>*- \c ->
--               return $ writeN (n, c))
--              () ()
-- tes'3 = mapM_ putStrLn . map fst . fst $
--   manyResults 20 (choose [5..]     >>- \n ->
--               choose (take n ['A'..]) >>- \c ->
--               return $ writeN (n, c))
--              () ()
chooseN :: MonadLogic m => Int -> m (Int,Int)
chooseN n = choose (map (n,) [1..])

-- mon1 :: Backtr () () Int
-- mon1 = vertical $ fmap chooseN (choose [5,6,7])
-- mon2 :: Backtr () () Int
-- mon2 = (choose [5,6,7]) >>|| chooseN
-- t = map fst $ fst $ manyResults 20 mon1 () ()

