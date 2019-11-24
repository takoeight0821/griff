{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Griff.ToFlat (convert) where

import           Control.Effect.Fresh
import           Control.Effect.State
import           Control.Monad
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Language.Griff.Core         as C
import           Language.Griff.FlatCore     as F
import           Language.Griff.Id
import           Language.Griff.Prelude
import           Language.Griff.Typing.Monad hiding (fresh)

data ToFlatEnv = ToFlatEnv
  { record  :: Set [Text] -- ソート済みのキーリストの集合
  , variant :: Map Text Int
  , defs    :: [F.ScDef]
  }

-- すでにレコードのキーのリストが登録済みなら、それをもとに値をソートする
-- 未登録なら、登録して値をソートする
sortRecord :: (Has (State ToFlatEnv) sig m) => [(Text, C.Exp)] -> m [C.Exp]
sortRecord r = do
  set <- gets record
  when (keys `notElem` set) $
    modify (\env -> env { record = record env <> Set.singleton keys })
  pure $ map snd r'
  where
    r' = List.sortOn fst r
    keys = map fst r'

-- タグが登録済みならそれを返す
-- そうでなければ新しいタグを生成して返す
variantTag :: (Has Fresh sig m, Has (State ToFlatEnv) sig m) => Text -> m Int
variantTag tag = do
  tagMap <- gets variant
  case Map.lookup tag tagMap of
    Nothing -> do
      i <- fresh
      modify (\env -> env { variant = Map.insert tag i tagMap })
      pure i
    Just i -> pure i

convert :: InferEff sig m => Toplevel -> m [F.ScDef]
convert (Toplevel scdefs) = undefined
