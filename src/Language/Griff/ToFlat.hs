module Language.Griff.ToFlat (convert) where

import           Control.Effect
import           Control.Effect.State
import           Language.Griff.Core     as C
import           Language.Griff.FlatCore as F
import           Language.Griff.Id


convert :: Toplevel -> [ScDef]
convert (Toplevel scdefs) = undefined
