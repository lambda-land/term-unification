module Unify
    ( unify
    , search
    , tes
    ) where

import qualified Control.Monad.State as State

import Control.Monad.Logic.Class (MonadLogic, msplit, (>>-))

import Control.Applicative (empty, (<|>))
import Control.Monad (forM_, guard, msum)

import Logic.Proof (Proof (..), pattern Proof)

import Control.Monad.Backtrack (allResults,interleaveMany,choose,manyResults, (>>||), (>>*-), alternateMany, vertical, interSequence, interMapM, chooseMapM)
import Common
import Term
import Rules (Rule (..), Rules, instantiate)
import GHC.IO (unsafePerformIO)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = pure False
anyM p (x : xs) = do c <- p x
                     if c then pure True
                          else anyM p xs

-- interleaveMany :: MonadLogic m => [m a] -> m a
-- interleaveMany = step [] where
--   step []  []       = empty
--   step ts' []       = step [] (reverse ts')
--   step ts' (t : ts) = do
--     mb <- msplit t
--     case mb of
--       Nothing      ->            step       ts'  ts
--       Just (a, t') -> pure a <|> step (t' : ts') ts

-- choose :: MonadLogic m => [a] -> m a
-- choose = interleaveMany . map pure

occurs :: Var -> Term -> Unif Bool
occurs v t = do
  t' <- inspect t
  case t' of
    Var v'    -> pure (v == v')
    Term _ ts -> anyM (occurs v) ts

-- fairer conjunction (>>- is still not fair enough)
(>>≠) :: MonadLogic m => m a -> (a -> m b) -> m b
a >>≠ k = step [] [] (Just a) where
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

search :: Rules -> Judgement -> Unif (Proof Judgement)
search rules j =
  -- find matching rules
  (choose rules                >>= \(_ {- name -}, rule) ->
   instantiate rule            >>= \rule' ->
   unify j (conclusion rule')  >>
   pure rule')
  -- bind with >>- because we want to try each matching rule in parallel
                               >>*- \rule' ->
  chooseMapM (search rules) (premises rule')
                               >>= \premiseTrees ->
  inspectDeep j                >>= \j' ->
  pure (Proof j' premiseTrees)

eachRule :: Rules -> Judgement -> Unif Rule
eachRule rs j = do
  (_,rule) <- choose rs -- msum (map pure rs)
  rule' <- instantiate rule
  unify j (conclusion rule')
  pure rule'

searchWithRule :: Rules -> Judgement -> Rule -> Unif (Proof Judgement)
searchWithRule rs j rule = do
  premiseTrees <- chooseMapM (search' rs) (premises rule)
  j' <- inspectDeep j 
  pure (Proof j' premiseTrees)

search' :: Rules -> Judgement -> Unif (Proof Judgement)
search' rs j = eachRule rs j >>*- searchWithRule rs j

tes :: IO ()
tes = print . map fst . fst $
  allResults (choose [5, 6, 7, 8] >>≠ \n ->
                         choose (take n ['A'..]) >>- \c ->
                         pure (n, c))
             () ()


tesRS :: Rules
tesRS = [
  -- ("ru1", Rule (Term "friends" [Term "iain" [],Term "kassia" []]) []),
        --  ("ru2", Rule (Term "friends" [Term "kassia" [],Term "grace" []]) []),
        --  ("ru3", Rule (Term "friends" [Term "grace" [],Term "ron" []]) []),
        --  ("ru4", Rule (Term "friends" [Term "ron" [],Term "kelli" []]) []),
        --  ("ru4_", Rule (Term "friends" [Term "kelli" [],Term "grace" []]) []),
        --  ("ru6", Rule (Term "friends" [Var "X",Var "Y"]) [Term "friends" [Var "Y",Var "X"]]),
        --  ("ru5", Rule (Term "friends" [Var "X",Var "Y"]) [Term "friends" [Var "X",Var "Z"],Term "friends" [Var "Z",Var "Y"]]),
        --  ("ru6*", Rule (Term "friends" [Var "X",Var "Y"]) [Term "friends" [Var "Y",Var "X"]]),
          ("lt_base", Rule (Term "lt" [Var "N",Term "succ" [Var "N"]]) [Term "nat" [Var "N"]]),
          ("lt_succ", Rule (Term "lt" [Var "N",Term "succ" [Var "M"]]) [Term "lt" [Var "N",Var "M"]]),
          ("lt_trans", Rule (Term "lt" [Var "N",Var "M"]) [Term "lt" [Var "N",Var "K"],Term "lt" [Var "K",Var "M"]]),

          ("nat_0", Rule (Term "nat" [Nat 0]) []),
          ("nat_succ", Rule (Term "nat" [Term "succ" [Var "N"]]) [Term "nat" [Var "N"]])
         ]

tesJ :: Judgement
-- tesJ = Term "friends" [Term "iain" [],Term "kelli" []]
tesJ = Term "lt" [Nat 1, Nat 4]
-- tesJ = Term "lt" [Var "X", Term "succ" [Var "X"]]
tesP :: Int -> IO ()
tesP n = print . (\(as,g) -> map (\(a,s) -> (a,metaCounter s,g)) as) $
  manyResults n (search' tesRS tesJ) emptyLocalState ()