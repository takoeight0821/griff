module Language.Griff.Reducer.SynToLIR where

import qualified Language.Griff.IR.Syntax as S
import qualified Language.Griff.IR.LIR as L

synToLIR :: S.Exp a -> L.Exp
synToLIR (S.Int _ x) = L.Int x
