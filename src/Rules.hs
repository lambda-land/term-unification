module Rules
    ( Rule (..), Rules
    , instantiate
    ) where

import Control.Monad (forM)

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Term

import Common
import Data.List (intercalate)

data Rule = Rule
  { conclusion :: !Judgement
  , premises   :: ![Judgement]
  }

instance Show Rule where
  show (Rule c ps) = show c ++ " :- " ++ intercalate ", " (map show ps)

type Rules = [( String -- name
              , Rule )]


instantiate :: Rule -> Unif Rule
instantiate (Rule conclusion premises) = do
  let vs = Set.toList $ Set.unions (vars conclusion : map vars premises)
  assoc <- Map.fromList <$> forM vs (\v -> do v' <- newMeta v
                                              pure (v, v'))
  let convert = mapVars (assoc Map.!)
  pure $ Rule (convert conclusion) (map convert premises)
