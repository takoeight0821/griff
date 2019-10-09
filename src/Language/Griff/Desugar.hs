{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Language.Griff.Desugar where

import           Data.Bifunctor
import           Data.Maybe
import           Language.Griff.Core         as C
import           Language.Griff.Id
import qualified Language.Griff.Syntax       as S
import           Language.Griff.TypeRep
import           Language.Griff.Typing.Infer (convertType)
import           Language.Griff.Typing.Monad

desugar :: [S.Dec Id] -> Env -> Toplevel
desugar ds env =
  let scDefs' = map dsScDef scDefs
      typeDefs' = map dsTypeAliasDef typeDefs
  in Toplevel scDefs' env typeDefs'
  where
    scDefs = mapMaybe (\case
                          S.ScDef _ f xs e -> Just (f, xs, e)
                          _ -> Nothing) ds
    typeDefs = mapMaybe (\case
                            S.TypeAliasDef _ n xs t -> Just (n, xs, t)
                            _ -> Nothing) ds

dsScDef :: (Id, [Id], S.Exp Id) -> (Id, Exp)
dsScDef (f, ps, e) = (f, foldr Lambda (dsExp e) ps)

dsExp :: S.Exp Id -> Exp
dsExp (S.Var _ x)    = Var x
dsExp (S.Int _ x)    = Int x
dsExp (S.Char _ x)   = Char x
dsExp (S.String _ x) = String x
dsExp (S.Bool _ x)   = Bool x
dsExp (S.Record _ xs) = Record $ map (second dsExp) xs
dsExp (S.Ascribe _ (S.Proj _ l v) t) =
  let TVariant t' = convertType t
  in Proj l (dsExp v) t'
dsExp (S.Ascribe _ e _) = dsExp e
dsExp S.Proj{} = error "unreachable S.Proj"
dsExp (S.Apply _ e1 e2) = Apply (dsExp e1) (dsExp e2)
dsExp (S.Lambda _ x e) = Lambda x $ dsExp e
dsExp (S.Let _ f xs e1 e2) = Let f (foldr Lambda (dsExp e1) xs) (dsExp e2)
dsExp (S.LetRec _ xs e) =
  LetRec (map (\(f, ps, e1) -> (f, foldr Lambda (dsExp e1) ps)) xs) (dsExp e)
dsExp (S.BinOp _ op e1 e2) =
  Prim (dsOp op) $ map dsExp [e1, e2]
  where
    dsOp S.Add = Add
    dsOp S.Sub = Sub
    dsOp S.Mul = Mul
    dsOp S.Div = Div
    dsOp S.Mod = Mod
    dsOp S.Eq  = Eq
    dsOp S.Neq = Neq
    dsOp S.Lt  = Lt
    dsOp S.Le  = Le
    dsOp S.Gt  = Gt
    dsOp S.Ge  = Ge
    dsOp S.And = And
    dsOp S.Or  = Or
dsExp (S.Case _ v cs) =
  Case (dsExp v) (map (bimap dsPat dsExp) cs)
dsExp (S.If _ c t f) =
  Case (dsExp c) [(BoolP True, dsExp t), (BoolP False, dsExp f)]

dsPat :: S.Pat Id -> Pat
dsPat (S.VarP _ x) = VarP x
dsPat (S.RecordP _ xs) = RecordP $ map (second dsPat) xs
dsPat (S.VariantP _ label pat ty) =
  let TVariant ty' = convertType ty
  in VariantP label (dsPat pat) ty'

dsTypeAliasDef :: (Id, [Id], S.Type Id) -> (Id, [Id], Ty)
dsTypeAliasDef (n, ps, t) = (n, ps, convertType t)
