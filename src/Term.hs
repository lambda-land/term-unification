{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Term where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate)

type Name = String
type Var = Name

data Term 
  = Var Var
  | Term Name [Term]
  deriving Eq

pattern Lit :: Name -> Term
pattern Lit c = Term c []

pattern Nat :: Int -> Term
pattern Nat n <- (termNat -> Just n)
  where Nat n = natTerm n

instance Show Term where
  show (Var v)     = "{" ++ v ++ "}"
  show (Nat n)     = show n
  show (Lit c)     = c
  show (Term c ts) = c ++ "(" ++ intercalate "," (map show ts) ++ ")"

openTerm :: Term -> Bool
openTerm (Var _)     = True
openTerm (Term _ ts) = any openTerm ts

closedTerm :: Term -> Bool
closedTerm = not . openTerm

vars :: Term -> Set Var
vars (Var v)     = Set.singleton v
vars (Term _ ts) = Set.unions (map vars ts)

mapVars :: (Var -> Var) -> Term -> Term
mapVars f (Var v)     = Var (f v)
mapVars f (Term c ts) = Term c (map (mapVars f) ts)

termNat :: Term -> Maybe Int
termNat (Var _) = Nothing
termNat (Lit "z") = Just 0
termNat (Term "succ" [t]) = (1 +) <$> termNat t
termNat _ = Nothing

natTerm :: Int -> Term
natTerm 0 = Lit "z"
natTerm n | n > 0     = Term "succ" [natTerm (n - 1)]
          | otherwise = error "natTerm: negative argument"

termList :: Term -> Maybe [Term]
termList (Var _) = Nothing
termList (Lit "nil") = Just []
termList (Term "cons" [x, xs]) = (x :) <$> termList xs
termList _ = Nothing

