{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.FreeVar (freevars) where

import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Language.Griff.Core
import           Language.Griff.Id
import           Language.Griff.Prelude

freevars :: Exp -> Set Id
freevars (Var x)       = Set.singleton x
freevars Int{}         = Set.empty
freevars Char{}        = Set.empty
freevars String{}      = Set.empty
freevars Bool{}        = Set.empty
freevars (Record xs)   = mconcat $ map (freevars . snd) xs
freevars (Proj _ e _)  = freevars e
freevars (Apply e1 e2) = freevars e1 <> freevars e2
freevars (Lambda x e)  = Set.delete x (freevars e)
freevars (Let x e1 e2) = freevars e1 <> Set.delete x (freevars e2)
freevars (LetRec xs e) = deletes (map fst xs) (mconcat (map (freevars . snd) xs) <> freevars e)
freevars (BinOp _ es) = mconcat $ map freevars es
freevars (Case e xs) = freevars e <> mconcat (map (uncurry deletes . bimap fvPat freevars) xs)

fvPat :: Pat -> [Id]
fvPat (VarP x)         = [x]
fvPat BoolP{}          = []
fvPat (RecordP xs)     = mconcat $ map (fvPat . snd) xs
fvPat (VariantP _ p _) = fvPat p

deletes :: (Foldable t, Ord a) => t a -> Set a -> Set a
deletes xs set = foldr Set.delete set xs
