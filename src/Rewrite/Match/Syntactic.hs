module Rewrite.Match.Syntactic where

import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.Map as Map
import Prelude hiding (const)

-- Term

data Term = Var String | Symbol String [Term]
  deriving (Eq, Ord)

instance Show Term where
  show (Var c) = "~" ++ c
  show (Symbol f []) = f
  show (Symbol f args) = f ++ show args

var = Var

func = Symbol

const f = Symbol f []

-- Substitution

newtype Substitution = Substitution (Map.Map Term Term)

emptySub = Substitution Map.empty

showPair (p, s) = show p ++ " ~> " ++ show s

curly str = "{ " ++ str ++ " }"

instance Show Substitution where
  show (Substitution subs) =
    curly . intercalate ", " . map showPair . Map.toList $ subs

combine :: Substitution -> Substitution -> Maybe Substitution
combine (Substitution sub) (Substitution sub') =
  if substitutionConflict
    then Nothing
    else Just . Substitution $ Map.union sub sub'
  where
    -- True, eg, when a single variable has multiple different substitutions
    substitutionConflict = or $ Map.intersectionWith (/=) sub sub'

-- Matching

syntacticMatch :: Substitution -> Term -> Term -> Maybe Substitution
syntacticMatch subs pattern@(Var _) subject =
  Just . Substitution $ Map.singleton pattern subject
syntacticMatch subs (Symbol p ps) (Symbol s ss)
  | p == s && length ps == length ss =
    foldM step subs (zip ps ss)
  where
    step subs (p, s) = syntacticMatch subs p s >>= combine subs
syntacticMatch _ _ _ = Nothing

match :: Term -> Term -> Maybe Substitution
match = syntacticMatch emptySub
