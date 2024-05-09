module Rules
    ( Rule (..), Rules
    , instantiate
    ) where

import Control.Monad (forM)

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Common

data Rule = Rule
  { conclusion :: !Judgement
  , premises   :: ![Judgement]
  }

type Rules = [( String -- name
              , Rule )]

vars :: Term -> Set Var
vars (Var v)     = Set.singleton v
vars (Term _ ts) = Set.unions (map vars ts)

mapVars :: (Var -> Var) -> Term -> Term
mapVars f (Var v)     = Var (f v)
mapVars f (Term c ts) = Term c (map (mapVars f) ts)

instantiate :: Rule -> Unif Rule
instantiate (Rule conclusion premises) = do
  let vs = Set.toList $ Set.unions (vars conclusion : map vars premises)
  assoc <- Map.fromList <$> forM vs (\v -> do v' <- newMeta v
                                              pure (v, v'))
  let convert = mapVars (assoc Map.!)
  pure $ Rule (convert conclusion) (map convert premises)
